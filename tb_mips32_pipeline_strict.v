// -------------------------------------------------------
// tb_mips32_pipeline_strict.v
// Self-checking testbench for mips32_pipeline
// - Success: store FINISH_DATA to FINISH_ADDR
// - Strict checks: unaligned load/store, IMEM/DMEM out-of-range,
//   store while EX/MEM invalid, writeback to $zero
//-------------------------------------------------------
`timescale 1ns/10ps

module tb_mips32_pipeline_strict #(parameter WIDTH=32, REGBITS=5) ();
  // ---- Parameters ----------------------------------------------------------
  localparam [WIDTH-1:0] DEFAULT_FINISH_ADDR = 32'd252; // 0xFC
  localparam [WIDTH-1:0] DEFAULT_FINISH_DATA = 32'd13;
  localparam integer     IMEM_WORDS = 256;  // ROM[0..255]
  localparam integer     DMEM_WORDS = 256;  // RAM[0..255]
  localparam real        STEP       = 10.0; // 100 MHz
  localparam integer     MAX_CYCLES = 5000;

  // ---- DUT I/F -------------------------------------------------------------
  reg                  clk;
  reg                  reset;
  wire [WIDTH-1:0]     imem_data;
  wire [WIDTH-1:0]     imem_addr;
  wire [WIDTH-1:0]     dmem_rdata;
  wire                 dmem_we;
  wire [WIDTH-1:0]     dmem_addr;
  wire [WIDTH-1:0]     dmem_wdata;

  // Performance counters
  wire [31:0] cycle_count;
  wire [31:0] inst_count;
  wire [31:0] branch_predict_correct;
  wire [31:0] branch_predict_total;

  // ---- Finish condition (plusargs) -----------------------------------------
  reg [WIDTH-1:0] FINISH_ADDR, FINISH_DATA;
  initial begin
    FINISH_ADDR = DEFAULT_FINISH_ADDR;
    FINISH_DATA = DEFAULT_FINISH_DATA;
    void'($value$plusargs("FINISH_ADDR=%d", FINISH_ADDR));
    void'($value$plusargs("FINISH_DATA=%d", FINISH_DATA));
    $display("FINISH condition: addr=%0d data=%0d", FINISH_ADDR, FINISH_DATA);
  end

  // ---- DUT -----------------------------------------------------------------
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

  // ---- Simple memories -----------------------------------------------------
  exmemory_imem #(WIDTH) imem(.adr(imem_addr), .memdata(imem_data));
  exmemory_dmem #(WIDTH) dmem(
    .clk(clk), .memwrite(dmem_we),
    .adr(dmem_addr), .writedata(dmem_wdata), .memdata(dmem_rdata)
  );

  // ---- Clock / Reset / Dumps ----------------------------------------------
  initial clk = 1'b0;
  always #(STEP/2.0) clk = ~clk;

  initial begin
    $dumpfile("dump_pipeline.vcd");
    $dumpvars(0, tb_mips32_pipeline_strict.dut);
  end

  initial begin
    reset = 1'b1;
    #(STEP*2.2) reset = 1'b0;
  end

  // ---- Timeout -------------------------------------------------------------
  initial begin : TIMEOUT
    #(STEP*MAX_CYCLES);
    $display("TIMEOUT: %0d cycles reached without success.", MAX_CYCLES);
    print_stats();
    $finish;
  end

  // ---- Helpers -------------------------------------------------------------
  task print_stats;
    real cpi;
    begin
      cpi = (inst_count != 0) ? (cycle_count * 1.0 / inst_count) : 0.0;
      $display("Final performance statistics:");
      $display("  Total cycles:            %0d", cycle_count);
      $display("  Instructions completed:  %0d", inst_count);
      $display("  Branch predictions:      %0d / %0d (correct/total)",
               branch_predict_correct, branch_predict_total);
      $display("  CPI (approx):            %0f", cpi);
    end
  endtask

  task fatal(input [1023:0] msg);
    begin
      $display("ERROR: %0s @cycle %0d", msg, cycle_count);
      print_stats();
      $finish;
    end
  endtask

  // ---- Success detection (watch DMEM stores) -------------------------------
  always @(negedge clk) begin
    if (!reset && dmem_we) begin
      $display("Cycle %0d: STORE data[%0d] -> addr[%0d]", cycle_count, dmem_wdata, dmem_addr);
      if ((dmem_addr == FINISH_ADDR) && (dmem_wdata == FINISH_DATA)) begin
        $display("SUCCESS: Reached finish condition: addr=%0d data=%0d", FINISH_ADDR, FINISH_DATA);
        print_stats();
        $finish;
      end
    end
  end

  // ---- Baseline self-checks (internal signal access) -----------------------
  // 1) No MEM write when EX/MEM stage invalid
  always @(negedge clk) begin
    if (!reset && dmem_we && !tb_mips32_pipeline_strict.dut.exmem_valid)
      fatal("dmem_we asserted while exmem_valid=0");
  end

  // 2) Forbid writeback to $zero
  always @(negedge clk) begin
    if (!reset &&
        tb_mips32_pipeline_strict.dut.memwb_regwrite &&
        (tb_mips32_pipeline_strict.dut.memwb_writereg == 5'd0))
      fatal("writeback to $zero detected");
  end

  // ---- STRICT checks -------------------------------------------------------
  // A) IMEM out-of-range fetch
  always @(posedge clk) begin
    if (!reset && (imem_addr[31:2] >= IMEM_WORDS))
      fatal({"IMEM fetch out of range: addr=", itoa(imem_addr)});
  end

  // B) Unaligned DMEM store
  always @(negedge clk) begin
    if (!reset && dmem_we && (dmem_addr[1:0] != 2'b00))
      fatal({"Unaligned store: addr=", itoa(dmem_addr)});
  end

  // C) Unaligned DMEM load  (detect via EX/MEM memtoreg)
  always @(negedge clk) begin
    if (!reset &&
        tb_mips32_pipeline_strict.dut.exmem_memtoreg &&
        tb_mips32_pipeline_strict.dut.exmem_valid &&
        (dmem_addr[1:0] != 2'b00))
      fatal({"Unaligned load: addr=", itoa(dmem_addr)});
  end

  // D) DMEM out-of-range store
  always @(negedge clk) begin
    if (!reset && dmem_we && (dmem_addr[31:2] >= DMEM_WORDS))
      fatal({"DMEM store out of range: addr=", itoa(dmem_addr)});
  end

  // E) DMEM out-of-range load
  always @(negedge clk) begin
    if (!reset &&
        tb_mips32_pipeline_strict.dut.exmem_memtoreg &&
        tb_mips32_pipeline_strict.dut.exmem_valid &&
        (dmem_addr[31:2] >= DMEM_WORDS))
      fatal({"DMEM load out of range: addr=", itoa(dmem_addr)});
  end

  // ---- Tiny decimal printer for addresses ---------------------------------
  function [256*8-1:0] itoa(input [31:0] v);
    integer d0,d1,d2,d3,d4,d5,d6,d7,d8,d9;
    begin
      d9=(v/1_000_000_000)%10; d8=(v/100_000_000)%10; d7=(v/10_000_000)%10;
      d6=(v/1_000_000)%10;     d5=(v/100_000)%10;     d4=(v/10_000)%10;
      d3=(v/1_000)%10;         d2=(v/100)%10;         d1=(v/10)%10; d0=v%10;
      itoa = { "0"+d9, "0"+d8, "0"+d7, "0"+d6, "0"+d5, "0"+d4, "0"+d3, "0"+d2, "0"+d1, "0"+d0 };
    end
  endfunction

endmodule

// ---------------------------------------------------------------------------
// Instruction memory (+HEX plusarg, default fib32.dat)
// ---------------------------------------------------------------------------
module exmemory_imem #(parameter WIDTH=32) (
  input  [WIDTH-1:0] adr,
  output reg [WIDTH-1:0] memdata
);
  reg [31:0] ROM [0:255];
  string hexfile;
  integer nwords;
  initial begin
    if (!$value$plusargs("HEX=%s", hexfile)) begin
      hexfile = "fib32.dat";
    end
    if (!$value$plusargs("WORDS=%d", nwords)) begin
      nwords = 0; // 0 = full range
    end
    $display("IMEM: loading HEX file: %s", hexfile);
    if (nwords > 0) begin
      $readmemh(hexfile, ROM, 0, nwords-1);
    end else begin
      $readmemh(hexfile, ROM);
    end
  end
  always @(*) memdata = ROM[adr>>2];
endmodule

// ---------------------------------------------------------------------------
// Data memory (sync write / async read)
// ---------------------------------------------------------------------------
module exmemory_dmem #(parameter WIDTH=32) (
  input                   clk,
  input                   memwrite,
  input  [WIDTH-1:0]      adr,
  input  [WIDTH-1:0]      writedata,
  output reg [WIDTH-1:0]  memdata
);
  reg [31:0] RAM [0:255];
  integer i;
  initial begin
    for (i=0;i<256;i=i+1) RAM[i] = 32'd0;
  end
  always @(posedge clk) if (memwrite) RAM[adr>>2] <= writedata;
  always @(*) memdata = RAM[adr>>2];
endmodule
