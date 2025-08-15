//-------------------------------------------------------
// fib32_pipeline.v
// Testbench for mips32_pipeline processor - fibonacci test
//-------------------------------------------------------
`timescale 1ns/10ps

module top #(parameter WIDTH = 32, REGBITS = 5) ();
	reg clk;
	reg reset;
	
	// Pipeline interface signals
	wire [WIDTH-1:0] imem_data;
	wire [WIDTH-1:0] imem_addr;
	wire [WIDTH-1:0] dmem_rdata;
	wire dmem_we;
	wire [WIDTH-1:0] dmem_addr;
	wire [WIDTH-1:0] dmem_wdata;
	
	// Performance counters
	wire [31:0] cycle_count;
	wire [31:0] inst_count;
	wire [31:0] branch_predict_correct;
	wire [31:0] branch_predict_total;
	
	// 10nsec --> 100MHz
	parameter STEP = 10.0;
	
	// instantiate processor
	mips32_pipeline #(WIDTH, REGBITS) dut(
		.clk(clk),
		.reset(reset),
		.imem_data(imem_data),
		.imem_ready(1'b1), 
		.imem_addr(imem_addr),
		.dmem_rdata(dmem_rdata),
		.dmem_we(dmem_we),
		.dmem_addr(dmem_addr),
		.dmem_wdata(dmem_wdata),
		.cycle_count(cycle_count),
		.inst_count(inst_count),
		.branch_predict_correct(branch_predict_correct),
		.branch_predict_total(branch_predict_total)
	);
	
	// instruction memory
	exmemory_imem #(WIDTH) imem(
		.adr(imem_addr),
		.memdata(imem_data)
	);
	
	// data memory
	exmemory_dmem #(WIDTH) dmem(
		.clk(clk),
		.memwrite(dmem_we),
		.adr(dmem_addr),
		.writedata(dmem_wdata),
		.memdata(dmem_rdata)
	);
	
	// initialize test
	initial begin
		// dump waveform
		$dumpfile("dump_pipeline.vcd");
		$dumpvars(0, top.dut);
		// reset
		clk <= 0; reset <= 1; # 22; reset <= 0;
		// stop at 1,000 cycles
		#(STEP*1000);
		$display("Simulation timeout");
		$display("Final performance statistics:");
		$display("  Total cycles: %d", cycle_count);
		$display("  Instructions completed: %d", inst_count);
		$display("  Branch predictions: %d / %d (correct/total)", branch_predict_correct, branch_predict_total);
		$finish;
	end
	
	// generate clock
	always #(STEP / 2)
		clk <= ~clk;
		
	// monitor data memory writes
	always @(negedge clk) begin
		if (dmem_we) begin
			$display("Cycle %d: Data [%d] is stored in Address [%d]", cycle_count, dmem_wdata, dmem_addr);
			if ((dmem_addr == 252) && (dmem_wdata == 13)) begin
				$display("Simulation completely successful");
				$display("Final performance statistics:");
				$display("  Total cycles: %d", cycle_count);
				$display("  Instructions completed: %d", inst_count);
				$display("  Branch predictions: %d / %d (correct/total)", branch_predict_correct, branch_predict_total);
				$finish;
			end
		end
	end
endmodule

/* instruction memory for MIPS pipeline */
module exmemory_imem #(parameter WIDTH = 32) (
	input [WIDTH-1:0] adr,
	output reg [WIDTH-1:0] memdata
);
	reg [31:0] ROM [0:255];
	string hexfile; integer nwords;
	initial begin
		if (!$value$plusargs("HEX=%s", hexfile)) begin
			hexfile = "fib32.dat";
		end
		if (!$value$plusargs("WORDS=%d", nwords)) begin
			nwords = 0; // 0 = read full range (may warn if shorter)
		end
		$display("IMEM: loading HEX file: %s", hexfile);
		if (nwords > 0) begin
			$readmemh(hexfile, ROM, 0, nwords-1);
		end else begin
			$readmemh(hexfile, ROM);
		end
	end
	
	always @(*)
		memdata <= ROM[adr>>2];
endmodule

/* data memory for MIPS pipeline */
module exmemory_dmem #(parameter WIDTH = 32) (
	input clk,
	input memwrite,
	input [WIDTH-1:0] adr, writedata,
	output reg [WIDTH-1:0] memdata
);
	reg [31:0] RAM [0:255];
	integer i;
	
	initial begin
		for (i = 0; i < 256; i = i + 1)
			RAM[i] = 0;
	end
	
	// synchronous write
	always @(posedge clk)
		if (memwrite) 
			RAM[adr>>2] <= writedata;
	
	// asynchronous read
	always @(*)
		memdata <= RAM[adr>>2];
endmodule
