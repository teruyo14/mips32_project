// -------------------------------------------------------
// sum32_pipeline.v
// Testbench for mips32_pipeline processor - sum test
// Post-P&R version
// -------------------------------------------------------
`timescale 1ns/10ps

module top ();
reg clk;
reg reset;

// Pipeline interface signals
wire [31:0] imem_data;
wire [31:0] imem_addr;
wire [31:0] dmem_rdata;
wire dmem_we;
wire [31:0] dmem_addr;
wire [31:0] dmem_wdata;

// Performance counters (may not exist in post-P&R netlist)
wire [31:0] cycle_count;
wire [31:0] inst_count;
wire [31:0] branch_predict_correct;
wire [31:0] branch_predict_total;

// 10nsec --> 100MHz
parameter STEP = 10.0;

// Counter for cycle tracking (in case performance counters are removed)
reg [31:0] local_cycle_count;

// instantiate processor (no parameters for post-P&R netlist)
mips32_pipeline dut(
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
exmemory_imem imem(
    .adr(imem_addr),
    .memdata(imem_data)
);

// data memory
exmemory_dmem dmem(
    .clk(clk),
    .memwrite(dmem_we),
    .adr(dmem_addr),
    .writedata(dmem_wdata),
    .memdata(dmem_rdata)
);

// initialize test
initial begin
    // dump waveform
    $dumpfile("dump_pipeline_pr.vcd");
    $dumpvars(0, top);
    
    // Initialize
    local_cycle_count = 0;
    
    // reset
    clk <= 0; 
    reset <= 1; 
    #22; 
    reset <= 0;
    
    // stop at 1,000 cycles
    #(STEP*1000);
    $display("Simulation timeout");
    $display("Final performance statistics:");
    $display("  Total cycles: %d", local_cycle_count);
    `ifdef HAS_PERF_COUNTERS
        $display("  Instructions completed: %d", inst_count);
        $display("  Branch predictions: %d / %d (correct/total)", branch_predict_correct, branch_predict_total);
    `endif
    $finish;
end

// generate clock
always #(STEP / 2)
    clk <= ~clk;

// Local cycle counter
always @(posedge clk) begin
    if (reset)
        local_cycle_count <= 0;
    else
        local_cycle_count <= local_cycle_count + 1;
end

// monitor data memory writes
always @(negedge clk) begin
    if (dmem_we) begin
        $display("Cycle %d: Data [%d] is stored in Address [%d]", 
                 local_cycle_count, dmem_wdata, dmem_addr);
        if ((dmem_addr == 252) && (dmem_wdata == 210)) begin
            $display("Simulation completely successful");
            $display("Final performance statistics:");
            $display("  Total cycles: %d", local_cycle_count);
            `ifdef HAS_PERF_COUNTERS
                $display("  Instructions completed: %d", inst_count);
                $display("  Branch predictions: %d / %d (correct/total)", 
                         branch_predict_correct, branch_predict_total);
            `endif
            $finish;
        end
    end
end
endmodule

/* instruction memory for MIPS pipeline */
module exmemory_imem (
    input [31:0] adr,
    output reg [31:0] memdata
);
    reg [31:0] ROM [0:255];
    string hexfile; integer nwords;
    initial begin
        if (!$value$plusargs("HEX=%s", hexfile)) begin
            hexfile = "sum32.dat";
        end
        if (!$value$plusargs("WORDS=%d", nwords)) begin
            nwords = 0; // 0 = read full range (may warn if file shorter)
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
module exmemory_dmem (
    input clk,
    input memwrite,
    input [31:0] adr, writedata,
    output reg [31:0] memdata
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
