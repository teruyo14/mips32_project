//-------------------------------------------------------
// mips32_pipeline.v
// Fixed MIPS32 with proper HI/LO register handling for synthesis
//-------------------------------------------------------

module mips32_pipeline #(parameter WIDTH = 32, REGBITS = 5) (
	input			clk, reset,
	input [WIDTH-1:0] 	imem_data,
	input			imem_ready,
	output [WIDTH-1:0] 	imem_addr,
	input [WIDTH-1:0] 	dmem_rdata,
	output			dmem_we,
	output [WIDTH-1:0] 	dmem_addr,
	output [WIDTH-1:0] 	dmem_wdata,
	// Performance counters
	output [31:0]		cycle_count,
	output [31:0]		inst_count,
	output [31:0]		branch_predict_correct,
	output [31:0]		branch_predict_total
);

// Pipeline registers
// IF/ID
reg [31:0] ifid_pc;
reg [31:0] ifid_pc_plus4;
reg [31:0] ifid_instr;
reg        ifid_valid;
reg        ifid_predicted_taken;

// ID/EX
reg [31:0] idex_pc;
reg [31:0] idex_pc_plus4;
reg [31:0] idex_rd1, idex_rd2;
reg [31:0] idex_signimm;
reg [4:0]  idex_rs, idex_rt, idex_rd;
reg [4:0]  idex_shamt;
reg        idex_regwrite, idex_memtoreg, idex_memwrite;
reg        idex_alusrc, idex_regdst;
reg [3:0]  idex_alucont;
reg        idex_branch, idex_jump, idex_jr, idex_jal;
reg        idex_memread;
reg        idex_valid;
reg        idex_predicted_taken;
reg        idex_multiply, idex_divide;
reg        idex_mfhi, idex_mflo;
reg        idex_bne;
reg [31:0] idex_jump_target;
reg        idex_lui;

// EX/MEM
reg [31:0] exmem_aluresult;
reg [31:0] exmem_writedata;
reg [4:0]  exmem_writereg;
reg        exmem_regwrite, exmem_memtoreg, exmem_memwrite;
reg        exmem_branch, exmem_zero;
reg [31:0] exmem_branchtarget;
reg        exmem_valid;
reg        exmem_predicted_taken;
reg [31:0] exmem_pc_plus4;
reg [31:0] exmem_pc;
reg        exmem_bne;
reg        exmem_jal;

// MEM/WB
reg [31:0] memwb_readdata;
reg [31:0] memwb_aluresult;
reg [4:0]  memwb_writereg;
reg        memwb_regwrite, memwb_memtoreg;
reg        memwb_valid;

// Internal signals
wire        fetch_ready = imem_ready;
wire [31:0] pc_next, pc_plus4;
reg  [31:0] pc;
wire [31:0] signimm;
wire [3:0]  alucont;
wire        regwrite, memtoreg, memwrite, memread;
wire        alusrc, regdst, branch, jump, jr, jal;
wire        multiply, divide, mfhi, mflo;
wire        bne, lui;
wire [31:0] rd1, rd2;
wire [31:0] alu_src1, alu_src2, alu_result;
wire        zero;
wire [4:0]  write_reg;
wire [31:0] write_data;
wire [31:0] branch_target;

// Multiply/Divide unit signals - FIXED: Single HI/LO register pair
reg [31:0] hi_reg, lo_reg;
reg [31:0] mult_op1, mult_op2;
reg [31:0] div_op1, div_op2;
wire [63:0] mult_result;
wire [31:0] div_quotient, div_remainder;
reg mult_busy, div_busy;
reg [5:0] mult_counter, div_counter;

// Branch prediction signals
wire predict_taken;
wire [9:0] pc_index = pc[11:2];
reg [1:0] branch_predictor [0:1023];
wire actual_taken;
wire prediction_wrong;

// Hazard detection signals
wire        stall, flush;
wire        ifid_write, ifid_flush, pc_write;
wire        idex_flush, exmem_flush;
wire        branch_taken_ex;

// Forwarding signals
wire [1:0]  forward_a, forward_b;
wire [31:0] forward_rd1, forward_rd2;

// Jump target calculation
wire [31:0] jump_target;
assign jump_target = {ifid_pc_plus4[31:28], ifid_instr[25:0], 2'b00};

// Performance counters
reg [31:0] cycle_counter;
reg [31:0] instruction_counter;
reg [31:0] branch_correct_counter;
reg [31:0] branch_total_counter;

always @(posedge clk) begin
	if (reset) begin
		cycle_counter <= 0;
		instruction_counter <= 0;
		branch_correct_counter <= 0;
		branch_total_counter <= 0;
	end else begin
		cycle_counter <= cycle_counter + 1;
		if (memwb_valid)
			instruction_counter <= instruction_counter + 1;
		// Branch prediction accuracy
		if (exmem_branch && exmem_valid) begin
			branch_total_counter <= branch_total_counter + 1;
			if (!prediction_wrong)
				branch_correct_counter <= branch_correct_counter + 1;
		end
	end
end

assign cycle_count = cycle_counter;
assign inst_count = instruction_counter;
assign branch_predict_correct = branch_correct_counter;
assign branch_predict_total = branch_total_counter;

//=====================================
// Branch Predictor
//=====================================
integer i;
// branch_predictor is already declared above at line 98

// Wire for update index - must be declared before use in always block
wire [9:0] exmem_pc_index = exmem_pc[11:2];

// Unified branch predictor update - single always block
always @(posedge clk) begin
	if (reset) begin
		// Initialize predictor
		for (i = 0; i < 1024; i = i + 1)
			branch_predictor[i] <= 2'b10;  // Initialize to weakly taken
	end else if (exmem_branch && exmem_valid) begin
		// Update predictor
		if (actual_taken) begin
			// Branch was taken
			if (branch_predictor[exmem_pc_index] != 2'b11)
				branch_predictor[exmem_pc_index] <= branch_predictor[exmem_pc_index] + 1;
		end else begin
			// Branch not taken
			if (branch_predictor[exmem_pc_index] != 2'b00)
				branch_predictor[exmem_pc_index] <= branch_predictor[exmem_pc_index] - 1;
		end
	end
end

assign predict_taken = branch_predictor[pc_index][1];

//=====================================
// Hazard Detection Unit
//=====================================
hazard_detection_unit hdu(
	.idex_memread(idex_memread),
	.idex_rt(idex_rt),
	.ifid_rs(ifid_instr[25:21]),
	.ifid_rt(ifid_instr[20:16]),
	.mult_busy(mult_busy),
	.div_busy(div_busy),
	.idex_multiply(idex_multiply),
	.idex_divide(idex_divide),
	.mfhi(mfhi),
	.mflo(mflo),
	.multiply(multiply),
	.divide(divide),
	.stall(stall),
	.ifid_write(ifid_write),
	.pc_write(pc_write)
);

// Branch misprediction detection
assign actual_taken = exmem_branch & (exmem_bne ? !exmem_zero : exmem_zero);
assign prediction_wrong = exmem_branch & exmem_valid & 
                         (actual_taken != exmem_predicted_taken);

// Flush signals
wire branch_flush = branch_taken_ex;
wire jump_flush = (idex_jump && idex_valid) || (idex_jr && idex_valid);
assign flush = branch_flush || jump_flush;
assign ifid_flush = flush;
assign idex_flush = jump_flush;
assign exmem_flush = 0;

//=====================================
// Forwarding Unit
//=====================================
forwarding_unit fwu(
	.idex_rs(idex_rs),
	.idex_rt(idex_rt),
	.exmem_regwrite(exmem_regwrite),
	.exmem_rd(exmem_writereg),
	.memwb_regwrite(memwb_regwrite),
	.memwb_rd(memwb_writereg),
	.forward_a(forward_a),
	.forward_b(forward_b)
);

//=====================================
// Stage 1: Instruction Fetch (IF)
//=====================================
// PC control
assign pc_plus4 = pc + 4;

// PC next selection
assign pc_next = reset ? 32'h00000000 :
                branch_taken_ex ? branch_target :
                idex_jr && idex_valid ? forward_rd1 :
                idex_jump && idex_valid ? idex_jump_target :
                pc_plus4;

always @(posedge clk) begin
	if (reset)
		pc <= 0;
	else if (pc_write && fetch_ready)
		pc <= pc_next;
end

assign imem_addr = pc;

// IF/ID pipeline register
always @(posedge clk) begin
	if (reset) begin
		ifid_pc <= 0;
		ifid_pc_plus4 <= 0;
		ifid_instr <= 32'h00000000;  // NOP
		ifid_valid <= 0;
		ifid_predicted_taken <= 0;
	end else if (ifid_flush) begin
		// Flush - insert NOP
		ifid_pc <= 0;
		ifid_pc_plus4 <= 0;
		ifid_instr <= 32'h00000000;  // NOP
		ifid_valid <= 0;
		ifid_predicted_taken <= 0;
	end else if (ifid_write && fetch_ready) begin
		ifid_pc <= pc;
		ifid_pc_plus4 <= pc_plus4;
		ifid_instr <= imem_data;
		ifid_valid <= 1;
		// Only predict for branch instructions
		ifid_predicted_taken <= (imem_data[31:26] == 6'b000100 || 
		                        imem_data[31:26] == 6'b000101) ? predict_taken : 0;
	end
end

//=====================================
// Stage 2: Instruction Decode (ID)
//=====================================
// Enhanced control unit with LUI/ORI
control_enhanced_lui ctrl(
	.op(ifid_instr[31:26]),
	.funct(ifid_instr[5:0]),
	.regwrite(regwrite),
	.memtoreg(memtoreg),
	.memwrite(memwrite),
	.memread(memread),
	.alucont(alucont),
	.alusrc(alusrc),
	.regdst(regdst),
	.branch(branch),
	.jump(jump),
	.jr(jr),
	.jal(jal),
	.multiply(multiply),
	.divide(divide),
	.mfhi(mfhi),
	.mflo(mflo),
	.bne(bne),
	.lui(lui)
);

// Register file with WB-to-ID forwarding
wire [31:0] rf_rd1, rf_rd2;
wire wb_to_id_fwd_a, wb_to_id_fwd_b;

assign wb_to_id_fwd_a = memwb_regwrite && (memwb_writereg != 0) && 
                        (memwb_writereg == ifid_instr[25:21]);
assign wb_to_id_fwd_b = memwb_regwrite && (memwb_writereg != 0) && 
                        (memwb_writereg == ifid_instr[20:16]);

regfile #(WIDTH, REGBITS) rf(
	.clk(clk),
	.we3(memwb_regwrite),
	.ra1(ifid_instr[25:21]),
	.ra2(ifid_instr[20:16]),
	.wa3(memwb_writereg),
	.wd3(write_data),
	.rd1(rf_rd1),
	.rd2(rf_rd2)
);

assign rd1 = wb_to_id_fwd_a ? write_data : rf_rd1;
assign rd2 = wb_to_id_fwd_b ? write_data : rf_rd2;

// Sign/Zero extension
wire zero_extend;
assign zero_extend = (ifid_instr[31:26] == 6'b001101) ||  // ORI
                     (ifid_instr[31:26] == 6'b001100) ||  // ANDI
                     (ifid_instr[31:26] == 6'b001110);    // XORI

assign signimm = zero_extend ? {16'h0000, ifid_instr[15:0]} :
                               {{16{ifid_instr[15]}}, ifid_instr[15:0]};

// ID/EX pipeline register
always @(posedge clk) begin
	if (reset) begin
		// Clear all signals
		idex_pc <= 0;
		idex_pc_plus4 <= 0;
		idex_rd1 <= 0;
		idex_rd2 <= 0;
		idex_signimm <= 0;
		idex_rs <= 0;
		idex_rt <= 0;
		idex_rd <= 0;
		idex_shamt <= 0;
		idex_regwrite <= 0;
		idex_memtoreg <= 0;
		idex_memwrite <= 0;
		idex_memread <= 0;
		idex_alusrc <= 0;
		idex_regdst <= 0;
		idex_alucont <= 0;
		idex_branch <= 0;
		idex_jump <= 0;
		idex_jr <= 0;
		idex_jal <= 0;
		idex_valid <= 0;
		idex_predicted_taken <= 0;
		idex_multiply <= 0;
		idex_divide <= 0;
		idex_mfhi <= 0;
		idex_mflo <= 0;
		idex_bne <= 0;
		idex_jump_target <= 0;
		idex_lui <= 0;
	end else if (idex_flush || branch_taken_ex) begin
		// On flush or branch taken, clear all control signals
		idex_pc <= ifid_pc;
		idex_pc_plus4 <= ifid_pc_plus4;
		idex_rd1 <= 0;
		idex_rd2 <= 0;
		idex_signimm <= 0;
		idex_rs <= 0;
		idex_rt <= 0;
		idex_rd <= 0;
		idex_shamt <= 0;
		idex_regwrite <= 0;
		idex_memtoreg <= 0;
		idex_memwrite <= 0;
		idex_memread <= 0;
		idex_alusrc <= 0;
		idex_regdst <= 0;
		idex_alucont <= 0;
		idex_branch <= 0;
		idex_jump <= 0;
		idex_jr <= 0;
		idex_jal <= 0;
		idex_valid <= 0;
		idex_predicted_taken <= 0;
		idex_multiply <= 0;
		idex_divide <= 0;
		idex_mfhi <= 0;
		idex_mflo <= 0;
		idex_bne <= 0;
		idex_jump_target <= 0;
		idex_lui <= 0;
	end else if (stall) begin
		// On stall, insert bubble
		idex_regwrite <= 0;
		idex_memtoreg <= 0;
		idex_memwrite <= 0;
		idex_memread <= 0;
		idex_branch <= 0;
		idex_jump <= 0;
		idex_jr <= 0;
		idex_jal <= 0;
		idex_valid <= 0;
		idex_multiply <= 0;
		idex_divide <= 0;
		idex_mfhi <= 0;
		idex_mflo <= 0;
		idex_lui <= 0;
		// Keep data values
		idex_pc <= ifid_pc;
		idex_pc_plus4 <= ifid_pc_plus4;
		idex_jump_target <= jump_target;
	end else begin
		// Normal operation
		idex_pc <= ifid_pc;
		idex_pc_plus4 <= ifid_pc_plus4;
		idex_rd1 <= rd1;
		idex_rd2 <= rd2;
		idex_signimm <= signimm;
		idex_rs <= ifid_instr[25:21];
		idex_rt <= ifid_instr[20:16];
		idex_rd <= ifid_instr[15:11];
		idex_shamt <= ifid_instr[10:6];
		idex_regwrite <= regwrite;
		idex_memtoreg <= memtoreg;
		idex_memwrite <= memwrite;
		idex_memread <= memread;
		idex_alusrc <= alusrc;
		idex_regdst <= regdst;
		idex_alucont <= alucont;
		idex_branch <= branch;
		idex_jump <= jump;
		idex_jr <= jr;
		idex_jal <= jal;
		idex_valid <= ifid_valid;
		idex_predicted_taken <= ifid_predicted_taken;
		idex_multiply <= multiply;
		idex_divide <= divide;
		idex_mfhi <= mfhi;
		idex_mflo <= mflo;
		idex_bne <= bne;
		idex_jump_target <= jump_target;
		idex_lui <= lui;
	end
end

//=====================================
// Stage 3: Execute (EX)
//=====================================
// Forwarding MUXes
mux3 #(WIDTH) forward_mux_a(
	.d0(idex_rd1),
	.d1(write_data),
	.d2(exmem_aluresult),
	.s(forward_a),
	.y(forward_rd1)
);

mux3 #(WIDTH) forward_mux_b(
	.d0(idex_rd2),
	.d1(write_data),
	.d2(exmem_aluresult),
	.s(forward_b),
	.y(forward_rd2)
);

// ALU source selection
assign alu_src1 = forward_rd1;
assign alu_src2 = idex_alusrc ? idex_signimm : forward_rd2;

// Enhanced ALU with LUI support
alu_enhanced_lui #(WIDTH) alu_unit(
	.a(alu_src1),
	.b(alu_src2),
	.shamt(idex_shamt),
	.alucont(idex_alucont),
	.hi_in(hi_reg),
	.lo_in(lo_reg),
	.mfhi(idex_mfhi),
	.mflo(idex_mflo),
	.lui(idex_lui),
	.signimm(idex_signimm),
	.result(alu_result),
	.zero(zero)
);

// Branch resolution in EX stage (early resolution)
assign branch_taken_ex = idex_branch & idex_valid & 
                        (idex_bne ? !zero : zero);

// FIXED: Single process for HI/LO registers and multiply/divide operations
assign mult_result = $signed(mult_op1) * $signed(mult_op2);
assign div_quotient = (div_op2 != 0) ? $signed(div_op1) / $signed(div_op2) : 32'hDEADBEEF;
assign div_remainder = (div_op2 != 0) ? $signed(div_op1) % $signed(div_op2) : 32'hDEADBEEF;

always @(posedge clk) begin
	if (reset) begin
		hi_reg <= 0;
		lo_reg <= 0;
		mult_busy <= 0;
		div_busy <= 0;
		mult_counter <= 0;
		div_counter <= 0;
		mult_op1 <= 0;
		mult_op2 <= 0;
		div_op1 <= 0;
		div_op2 <= 0;
	end else begin
		// Handle multiply
		if (idex_multiply && !mult_busy && !div_busy && idex_valid) begin
			mult_busy <= 1;
			mult_counter <= 6;
			mult_op1 <= forward_rd1;
			mult_op2 <= forward_rd2;
		end else if (mult_busy) begin
			if (mult_counter > 0) begin
				mult_counter <= mult_counter - 1;
			end else begin
				// Complete multiplication
				hi_reg <= mult_result[63:32];
				lo_reg <= mult_result[31:0];
				mult_busy <= 0;
			end
		end
		
		// Handle divide
		if (idex_divide && !div_busy && !mult_busy && idex_valid) begin
			div_busy <= 1;
			div_counter <= 12;
			div_op1 <= forward_rd1;
			div_op2 <= forward_rd2;
		end else if (div_busy) begin
			if (div_counter > 0) begin
				div_counter <= div_counter - 1;
			end else begin
				// Complete division
				hi_reg <= div_remainder;
				lo_reg <= div_quotient;
				div_busy <= 0;
			end
		end
	end
end

// Write register selection
wire [4:0] write_reg_normal;
assign write_reg_normal = idex_regdst ? idex_rd : idex_rt;
assign write_reg = (idex_jal && idex_valid) ? 5'd31 : write_reg_normal;

// Branch target calculation
assign branch_target = idex_pc + 4 + (idex_signimm << 2);

// ALU result selection for JAL
wire [31:0] alu_result_final;
assign alu_result_final = (idex_jal && idex_valid) ? idex_pc_plus4 : alu_result;

// EX/MEM pipeline register
always @(posedge clk) begin
	if (reset || exmem_flush) begin
		exmem_aluresult <= 0;
		exmem_writedata <= 0;
		exmem_writereg <= 0;
		exmem_regwrite <= 0;
		exmem_memtoreg <= 0;
		exmem_memwrite <= 0;
		exmem_branch <= 0;
		exmem_zero <= 0;
		exmem_branchtarget <= 0;
		exmem_valid <= 0;
		exmem_predicted_taken <= 0;
		exmem_pc_plus4 <= 0;
		exmem_pc <= 0;
		exmem_bne <= 0;
		exmem_jal <= 0;
	end else begin
		exmem_aluresult <= alu_result_final;
		exmem_writedata <= forward_rd2;
		exmem_writereg <= write_reg;
		exmem_regwrite <= idex_regwrite;
		exmem_memtoreg <= idex_memtoreg;
		exmem_memwrite <= idex_memwrite;
		exmem_branch <= idex_branch;
		exmem_zero <= zero;
		exmem_branchtarget <= branch_target;
		exmem_valid <= idex_valid;
		exmem_predicted_taken <= idex_predicted_taken;
		exmem_pc_plus4 <= idex_pc_plus4;
		exmem_pc <= idex_pc;
		exmem_bne <= idex_bne;
		exmem_jal <= idex_jal;
	end
end

//=====================================
// Stage 4: Memory (MEM)
//=====================================
assign dmem_we = exmem_memwrite && exmem_valid;
assign dmem_addr = exmem_aluresult;
assign dmem_wdata = exmem_writedata;

// MEM/WB pipeline register
always @(posedge clk) begin
	if (reset) begin
		memwb_readdata <= 0;
		memwb_aluresult <= 0;
		memwb_writereg <= 0;
		memwb_regwrite <= 0;
		memwb_memtoreg <= 0;
		memwb_valid <= 0;
	end else begin
		memwb_readdata <= dmem_rdata;
		memwb_aluresult <= exmem_aluresult;
		memwb_writereg <= exmem_writereg;
		// Additional protection against $0 writes
		memwb_regwrite <= exmem_regwrite && exmem_valid && (exmem_writereg != 0);
		memwb_memtoreg <= exmem_memtoreg;
		memwb_valid <= exmem_valid;
	end
end

//=====================================
// Stage 5: Write Back (WB)
//=====================================
assign write_data = memwb_memtoreg ? memwb_readdata : memwb_aluresult;

endmodule

//=====================================
// Enhanced Control Unit with LUI/ORI
//=====================================
module control_enhanced_lui (
	input [5:0]		op,
	input [5:0]		funct,
	output reg		regwrite,
	output reg		memtoreg,
	output reg		memwrite,
	output reg		memread,
	output reg [3:0]	alucont,
	output reg		alusrc,
	output reg		regdst,
	output reg		branch,
	output reg		jump,
	output reg		jr,
	output reg		jal,
	output reg		multiply,
	output reg		divide,
	output reg		mfhi,
	output reg		mflo,
	output reg		bne,
	output reg		lui
);

// Opcodes
parameter RTYPE = 6'b000000;
parameter LW    = 6'b100011;
parameter SW    = 6'b101011;
parameter BEQ   = 6'b000100;
parameter BNE   = 6'b000101;
parameter ADDI  = 6'b001000;
parameter ANDI  = 6'b001100;
parameter ORI   = 6'b001101;
parameter XORI  = 6'b001110;
parameter LUI   = 6'b001111;
parameter J     = 6'b000010;
parameter JAL   = 6'b000011;

// Function codes
parameter ADD   = 6'b100000;
parameter SUB   = 6'b100010;
parameter AND   = 6'b100100;
parameter OR    = 6'b100101;
parameter XOR   = 6'b100110;
parameter NOR   = 6'b100111;
parameter SLT   = 6'b101010;
parameter SLL   = 6'b000000;
parameter SRL   = 6'b000010;
parameter SRA   = 6'b000011;
parameter JR    = 6'b001000;
parameter MULT  = 6'b011000;
parameter DIV   = 6'b011010;
parameter MFHI  = 6'b010000;
parameter MFLO  = 6'b010010;

always @(*) begin
	// Default values
	regwrite = 0;
	memtoreg = 0;
	memwrite = 0;
	memread = 0;
	alucont = 4'b0000;
	alusrc = 0;
	regdst = 0;
	branch = 0;
	jump = 0;
	jr = 0;
	jal = 0;
	multiply = 0;
	divide = 0;
	mfhi = 0;
	mflo = 0;
	bne = 0;
	lui = 0;
	
	case (op)
		RTYPE: begin
			regdst = 1;
			case (funct)
				ADD: begin
					regwrite = 1;
					alucont = 4'b0010;
				end
				SUB: begin
					regwrite = 1;
					alucont = 4'b0110;
				end
				AND: begin
					regwrite = 1;
					alucont = 4'b0000;
				end
				OR: begin
					regwrite = 1;
					alucont = 4'b0001;
				end
				XOR: begin
					regwrite = 1;
					alucont = 4'b0011;
				end
				NOR: begin
					regwrite = 1;
					alucont = 4'b0100;
				end
				SLT: begin
					regwrite = 1;
					alucont = 4'b0111;
				end
				SLL: begin
					regwrite = 1;
					alucont = 4'b1000;
				end
				SRL: begin
					regwrite = 1;
					alucont = 4'b1001;
				end
				SRA: begin
					regwrite = 1;
					alucont = 4'b1010;
				end
				JR: begin
					jr = 1;
				end
				MULT: begin
					multiply = 1;
				end
				DIV: begin
					divide = 1;
				end
				MFHI: begin
					regwrite = 1;
					mfhi = 1;
					alucont = 4'b1100;
				end
				MFLO: begin
					regwrite = 1;
					mflo = 1;
					alucont = 4'b1101;
				end
				default: alucont = 4'b0000;
			endcase
		end
		LW: begin
			regwrite = 1;
			memtoreg = 1;
			memread = 1;
			alusrc = 1;
			alucont = 4'b0010; // add
		end
		SW: begin
			memwrite = 1;
			alusrc = 1;
			alucont = 4'b0010; // add
		end
		BEQ: begin
			branch = 1;
			alucont = 4'b0110; // sub
			bne = 0;
		end
		BNE: begin
			branch = 1;
			alucont = 4'b0110; // sub
			bne = 1;
		end
		ADDI: begin
			regwrite = 1;
			alusrc = 1;
			alucont = 4'b0010; // add
		end
		ANDI: begin
			regwrite = 1;
			alusrc = 1;
			alucont = 4'b0000; // and
		end
		ORI: begin
			regwrite = 1;
			alusrc = 1;
			alucont = 4'b0001; // or
		end
		XORI: begin
			regwrite = 1;
			alusrc = 1;
			alucont = 4'b0011; // xor
		end
		LUI: begin
			regwrite = 1;
			lui = 1;
			alucont = 4'b1111; // special for lui
		end
		J: begin
			jump = 1;
		end
		JAL: begin
			jump = 1;
			jal = 1;
			regwrite = 1;
		end
		default: begin
			// All outputs already set to 0
		end
	endcase
end
endmodule

//=====================================
// Enhanced ALU with LUI support
//=====================================
module alu_enhanced_lui #(parameter WIDTH = 32) (
	input [WIDTH-1:0]	a, b,
	input [4:0]		shamt,
	input [3:0]		alucont,
	input [WIDTH-1:0]	hi_in, lo_in,
	input			mfhi, mflo,
	input			lui,
	input [WIDTH-1:0]	signimm,
	output reg [WIDTH-1:0]	result,
	output			zero
);

wire [WIDTH-1:0] b2, sum, slt;

assign b2 = alucont[2] ? ~b : b;
assign sum = a + b2 + alucont[2];
assign slt = {31'b0, sum[WIDTH-1]};

always @(*)
	if (lui)
		result = {signimm[15:0], 16'h0000};  // LUI: load upper 16 bits
	else
		case (alucont)
			4'b0000: result = a & b;        // and
			4'b0001: result = a | b;        // or
			4'b0010: result = sum;          // add
			4'b0011: result = a ^ b;        // xor
			4'b0100: result = ~(a | b);     // nor
			4'b0110: result = sum;          // sub
			4'b0111: result = slt;          // slt
			4'b1000: result = b << shamt;   // sll
			4'b1001: result = b >> shamt;   // srl
			4'b1010: result = $signed(b) >>> shamt; // sra
			4'b1100: result = hi_in;        // mfhi
			4'b1101: result = lo_in;        // mflo
			default: result = 0;
		endcase

assign zero = (result == 0);
endmodule

//=====================================
// Hazard Detection Unit
//=====================================
module hazard_detection_unit (
	input		idex_memread,
	input [4:0]	idex_rt,
	input [4:0]	ifid_rs,
	input [4:0]	ifid_rt,
	input		mult_busy,
	input		div_busy,
	input		idex_multiply,
	input		idex_divide,
	input		mfhi,
	input		mflo,
	input		multiply,
	input		divide,
	output		stall,
	output		ifid_write,
	output		pc_write
);

// Detect load-use hazard
wire load_use_hazard;
assign load_use_hazard = idex_memread && 
                        ((idex_rt != 0) &&
                         ((idex_rt == ifid_rs) || (idex_rt == ifid_rt)));

// HI/LO structural hazards:
//  (a) If EX is starting MULT/DIV this cycle, block an ID-stage MFHI/MFLO from advancing.
//  (b) While MULT/DIV unit is busy, block any ID-stage MULT/DIV/MFHI/MFLO from advancing.
wire hilo_busy        = mult_busy || div_busy;
wire ex_hilo_start    = idex_multiply || idex_divide;
wire id_uses_hilo     = mfhi || mflo || multiply || divide;
wire hilo_struct_hazard = (ex_hilo_start && (mfhi || mflo)) ||
                          (hilo_busy && id_uses_hilo);

assign stall      = load_use_hazard || hilo_struct_hazard;
assign ifid_write = ~stall;
assign pc_write   = ~stall;

endmodule

//=====================================
// Forwarding Unit
//=====================================
module forwarding_unit (
	input [4:0]	idex_rs,
	input [4:0]	idex_rt,
	input		exmem_regwrite,
	input [4:0]	exmem_rd,
	input		memwb_regwrite,
	input [4:0]	memwb_rd,
	output reg [1:0] forward_a,
	output reg [1:0] forward_b
);

always @(*) begin
	// Default: no forwarding
	forward_a = 2'b00;
	forward_b = 2'b00;
	
	// Forward A (rs)
	if (idex_rs != 0) begin
		if (exmem_regwrite && (exmem_rd == idex_rs))
			forward_a = 2'b10;  // Forward from EX/MEM
		else if (memwb_regwrite && (memwb_rd == idex_rs))
			forward_a = 2'b01;  // Forward from MEM/WB
	end
	
	// Forward B (rt)
	if (idex_rt != 0) begin
		if (exmem_regwrite && (exmem_rd == idex_rt))
			forward_b = 2'b10;  // Forward from EX/MEM
		else if (memwb_regwrite && (memwb_rd == idex_rt))
			forward_b = 2'b01;  // Forward from MEM/WB
	end
end

endmodule

//=====================================
// 3-to-1 Multiplexer
//=====================================
module mux3 #(parameter WIDTH = 32) (
	input [WIDTH-1:0]	d0, d1, d2,
	input [1:0]		s,
	output reg [WIDTH-1:0]	y
);

always @(*)
	case (s)
		2'b00: y = d0;
		2'b01: y = d1;
		2'b10: y = d2;
		default: y = d0;
	endcase

endmodule

//=====================================
// Register File
//=====================================
module regfile #(parameter WIDTH = 32, REGBITS = 5) (
	input			clk,
	input			we3,
	input [4:0]		ra1, ra2, wa3,
	input [WIDTH-1:0]	wd3,
	output [WIDTH-1:0]	rd1, rd2
);

reg [WIDTH-1:0] RAM [(1<<REGBITS)-1:0];

// Synthesis-friendly initialization
always @(posedge clk) begin
	if (we3 && (wa3 != 0)) RAM[wa3] <= wd3;
end

assign rd1 = (ra1 != 0) ? RAM[ra1] : 0;
assign rd2 = (ra2 != 0) ? RAM[ra2] : 0;

endmodule
