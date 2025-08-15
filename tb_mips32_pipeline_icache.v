//-------------------------------------------------------
// tb_mips32_pipeline_icache.v
// Self-checking testbench for mips32_pipeline + icache_2way
// - Wraps the CPU and gates its clock on I$ miss (no DUT edits)
// - Success when store FINISH_DATA to FINISH_ADDR
//-------------------------------------------------------
`timescale 1ns/10ps

module tb_mips32_pipeline_icache #(parameter WIDTH=32, REGBITS=5) ();
  // ---- Parameters ---------------------------------------------------------
  localparam [WIDTH-1:0] DEFAULT_FINISH_ADDR = 32'd252; // 0xFC
  localparam [WIDTH-1:0] DEFAULT_FINISH_DATA = 32'd13;
  localparam real        STEP       = 10.0; // 100 MHz
  localparam integer     MAX_CYCLES = 5000;

  // ---- Clocks / Reset -----------------------------------------------------
  reg  clk, reset_core, reset_ic;
  initial clk = 1'b0; always #(STEP/2.0) clk = ~clk;

  // ---- DUT <-> I$ (CPU side) ---------------------------------------------
  wire [WIDTH-1:0] imem_addr_cpu;
  wire [WIDTH-1:0] imem_data_cpu;
  wire             icpu_ready;      // I$ has data this cycle
  wire             icpu_req  = 1'b1;
  wire             icpu_stall= 1'b0; // no backpressure from CPU side

  // ---- I$ <-> IMEM (memory side) -----------------------------------------
  wire [WIDTH-1:0] imem_addr;
  wire             imem_req;
  wire [WIDTH-1:0] imem_data;
  wire             imem_ready;

  // ---- DMEM (directly from DUT) ------------------------------------------
  wire [WIDTH-1:0] dmem_rdata;
  wire             dmem_we;
  wire [WIDTH-1:0] dmem_addr;
  wire [WIDTH-1:0] dmem_wdata;

  // ---- Perf counters ------------------------------------------------------
  wire [31:0] cycle_count;
  wire [31:0] inst_count;
  wire [31:0] branch_predict_correct;
  wire [31:0] branch_predict_total;

  // ---- Finish condition (+args) -------------------------------------------
  reg [WIDTH-1:0] FINISH_ADDR, FINISH_DATA;
  initial begin
    FINISH_ADDR = DEFAULT_FINISH_ADDR;
    FINISH_DATA = DEFAULT_FINISH_DATA;
    void'($value$plusargs("FINISH_ADDR=%d", FINISH_ADDR));
    void'($value$plusargs("FINISH_DATA=%d", FINISH_DATA));
    $display("FINISH condition: addr=%0d data=%0d", FINISH_ADDR, FINISH_DATA);
  end

  // ---- DUT ----------------------------------------------------------------
  mips32_pipeline #(WIDTH, REGBITS) dut(
    .clk(clk),
    .reset(reset_core),
    .imem_data(imem_data_cpu),
    .imem_ready(icpu_ready),
    .imem_addr(imem_addr_cpu),
    .dmem_rdata(dmem_rdata),
    .dmem_we(dmem_we),
    .dmem_addr(dmem_addr),
    .dmem_wdata(dmem_wdata),
    .cycle_count(cycle_count),
    .inst_count(inst_count),
    .branch_predict_correct(branch_predict_correct),
    .branch_predict_total(branch_predict_total)
  );

  // ---- I-cache ------------------------------------------------------------
  wire [31:0] hit_count, miss_count, stall_cycles;
  wire        debug_en  = 1'b0;
  wire [127:0] debug_info;

  icache_2way icache(
    .clk(clk),
    .reset(reset_ic),
    // CPU side
    .cpu_addr(imem_addr_cpu),
    .cpu_req(icpu_req),
    .cpu_data(imem_data_cpu),
    .cpu_ready(icpu_ready),
    .cpu_stall(icpu_stall),
    // Memory side
    .mem_addr(imem_addr),
    .mem_req(imem_req),
    .mem_data(imem_data),
    .mem_ready(imem_ready),
    // Stats / debug
    .hit_count(hit_count),
    .miss_count(miss_count),
    .stall_cycles(stall_cycles),
    .debug_en(debug_en),
    .debug_info(debug_info)
  );

  // ---- IMEM: async ROM, always-ready handshaking -------------------------
  exmemory_imem #(WIDTH) imem(
    .adr(imem_addr),
    .memdata(imem_data)
  );
  assign imem_ready = imem_req; // always ready when requested

  // ---- DMEM: sync write / async read -------------------------------------
  exmemory_dmem #(WIDTH) dmem(
    .clk(clk),
    .memwrite(dmem_we),
    .adr(dmem_addr),
    .writedata(dmem_wdata),
    .memdata(dmem_rdata)
  );

  // ---- Dumps / Reset / Timeout -------------------------------------------
  initial begin
    $dumpfile("dump_pipeline_icache.vcd");
    $dumpvars(0, tb_mips32_pipeline_icache.dut);
  end

  // Two-phase reset: release I$ first, then the core after I$ can serve the first fetch
  initial begin
    reset_core = 1'b1;
    reset_ic   = 1'b1;
    #(STEP*0.8);          // short reset pulse for I$
    reset_ic   = 1'b0;    // let the cache start filling line 0
    // Wait until I$ reports data ready at least once (first-line fill complete)
    wait (icpu_ready === 1'b1);
    #(STEP*0.8);
    reset_core = 1'b0;    // start the core on a full I$
  end

  initial begin : TIMEOUT
    #(STEP*MAX_CYCLES);
    $display("TIMEOUT: %0d cycles reached without success.", MAX_CYCLES);
    print_stats();
    $finish;
  end

  // ---- Success detection (watch DMEM store) -------------------------------
  always @(negedge clk) begin
    if (!reset_core && dmem_we) begin
      $display("Cycle %0d: STORE data[%0d] -> addr[%0d]", cycle_count, dmem_wdata, dmem_addr);
      if ((dmem_addr == FINISH_ADDR) && (dmem_wdata == FINISH_DATA)) begin
        $display("SUCCESS: Reached finish condition: addr=%0d data=%0d", FINISH_ADDR, FINISH_DATA);
        $display("I$ stats: hits=%0d misses=%0d stall_cycles=%0d", hit_count, miss_count, stall_cycles);
        print_stats();
        $finish;
      end
    end
  end

  // ---- Basic self-checks (keep them minimal since gating stalls core) ----
  always @(negedge clk) begin
    if (!reset_core && dmem_we && !tb_mips32_pipeline_icache.dut.exmem_valid) begin
      $display("ERROR: dmem_we asserted while exmem_valid=0 @cycle %0d", cycle_count);
      $finish;
    end
  end

  always @(negedge clk) begin
    if (!reset_core && tb_mips32_pipeline_icache.dut.memwb_regwrite && (tb_mips32_pipeline_icache.dut.memwb_writereg == 5'd0)) begin
      $display("ERROR: writeback to $zero detected @cycle %0d", cycle_count);
      $finish;
    end
  end

  // ---- Stats --------------------------------------------------------------
  task print_stats;
    real cpi;
    begin
      if (inst_count != 0) cpi = cycle_count * 1.0 / inst_count; else cpi = 0.0;
      $display("Final performance statistics:");
      $display("  Total cycles:            %0d", cycle_count);
      $display("  Instructions completed:  %0d", inst_count);
      $display("  Branch predictions:      %0d / %0d (correct/total)", branch_predict_correct, branch_predict_total);
      $display("  CPI (approx):            %0f", cpi);
    end
  endtask
endmodule

// ---------------------------------------------------------------------------
// Instruction memory (+HEX plusarg, default: fib32.dat)
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
