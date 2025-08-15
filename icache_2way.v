//-------------------------------------------------------
// icache_2way.v
// 2-Way Set Associative Instruction Cache
//-------------------------------------------------------

module icache_2way #(
    parameter ADDR_WIDTH = 32,
    parameter DATA_WIDTH = 32,
    parameter CACHE_SIZE = 256,      // 256 words = 1KB total
    parameter BLOCK_SIZE = 4,        // 4 words per block
    parameter NUM_WAYS = 2,          // 2-way set associative
    parameter NUM_SETS = 32,         // 32 sets (not 64!) 
    parameter SET_BITS = 5,          // log2(32) = 5 (not 6!)
    parameter OFFSET_BITS = 2,       // log2(4) = 2
    parameter BLOCK_OFFSET_BITS = 4  // log2(16) = 4
) (
    input                       clk,
    input                       reset,
    
    // CPU Interface
    input [ADDR_WIDTH-1:0]      cpu_addr,
    input                       cpu_req,
    output reg [DATA_WIDTH-1:0] cpu_data,
    output reg                  cpu_ready,
    input                       cpu_stall,
    
    // Memory Interface
    output reg [ADDR_WIDTH-1:0] mem_addr,
    output reg                  mem_req,
    input [DATA_WIDTH-1:0]      mem_data,
    input                       mem_ready,
    
    // Performance Counters
    output reg [31:0]           hit_count,
    output reg [31:0]           miss_count,
    output reg [31:0]           stall_cycles,
    
    // Debug Interface
    input                       debug_en,
    output [127:0]              debug_info
);

// Derived widths
localparam TAG_BITS   = ADDR_WIDTH - SET_BITS - BLOCK_OFFSET_BITS; // e.g., 32 - (5 + 4) = 23
localparam INDEX_BITS = 1 + SET_BITS + OFFSET_BITS;                // way + set + word_offset

// Cache Storage - 2 ways
reg [DATA_WIDTH-1:0] cache_data [0:CACHE_SIZE-1];  // Data array
reg [TAG_BITS-1:0] cache_tag [0:NUM_SETS-1][0:NUM_WAYS-1]; // Tag array
reg cache_valid [0:NUM_SETS-1][0:NUM_WAYS-1];      // Valid bits
reg cache_lru [0:NUM_SETS-1];                      // LRU bits (0=way0 recently used, 1=way1 recently used)

// Address Decomposition
wire [OFFSET_BITS-1:0] offset = cpu_addr[OFFSET_BITS+1:2];
wire [SET_BITS-1:0] set_index = cpu_addr[SET_BITS+BLOCK_OFFSET_BITS-1:BLOCK_OFFSET_BITS];
wire [TAG_BITS-1:0] tag = cpu_addr[ADDR_WIDTH-1:SET_BITS+BLOCK_OFFSET_BITS];

// Hit Detection for both ways
wire hit_way0 = cache_valid[set_index][0] && (cache_tag[set_index][0] == tag) && cpu_req;
wire hit_way1 = cache_valid[set_index][1] && (cache_tag[set_index][1] == tag) && cpu_req;
wire hit = hit_way0 || hit_way1;
wire hit_way = hit_way1; // 0 for way0, 1 for way1

// LRU victim selection
wire victim_way = cache_lru[set_index];

// FSM States
localparam IDLE = 2'b00;
localparam FETCH = 2'b01;
localparam WAIT_PIPELINE = 2'b10;

reg [1:0] state, next_state;
reg [1:0] word_cnt;
reg [ADDR_WIDTH-1:0] saved_addr;
reg pending_request;
reg saved_way;

// Saved address decomposition
wire [SET_BITS-1:0] saved_set = saved_addr[SET_BITS+BLOCK_OFFSET_BITS-1:BLOCK_OFFSET_BITS];
wire [OFFSET_BITS-1:0] saved_offset = saved_addr[OFFSET_BITS+1:2];

// Calculate cache data index
function [INDEX_BITS-1:0] get_cache_index;
    input [SET_BITS-1:0] set;
    input way;
    input [OFFSET_BITS-1:0] word_offset;
    begin
        get_cache_index = {way, set, word_offset};
    end
endfunction

// State Machine
always @(posedge clk) begin
    if (reset) begin
        state <= IDLE;
    end else begin
        state <= next_state;
    end
end

always @(*) begin
    next_state = state;
    case (state)
        IDLE: begin
            if (cpu_req && !hit && !cpu_stall) begin
                next_state = FETCH;
            end else if (cpu_req && hit && cpu_stall) begin
                next_state = WAIT_PIPELINE;
            end
        end
        
        FETCH: begin
            if (mem_ready && word_cnt == 3) begin
                if (cpu_stall) begin
                    next_state = WAIT_PIPELINE;
                end else begin
                    next_state = IDLE;
                end
            end
        end
        
        WAIT_PIPELINE: begin
            if (!cpu_stall) begin
                next_state = IDLE;
            end
        end
    endcase
end

// Main Logic
integer i, j;
always @(posedge clk) begin
    if (reset) begin
        // Reset all state
        cpu_data <= 0;
        cpu_ready <= 0;
        mem_req <= 0;
        mem_addr <= 0;
        word_cnt <= 0;
        saved_addr <= 0;
        saved_way <= 0;
        pending_request <= 0;
        hit_count <= 0;
        miss_count <= 0;
        stall_cycles <= 0;
        
        // Clear cache
        for (i = 0; i < NUM_SETS; i = i + 1) begin
            for (j = 0; j < NUM_WAYS; j = j + 1) begin
                cache_valid[i][j] <= 0;
                cache_tag[i][j] <= 0;
            end
            cache_lru[i] <= 0;
        end
    end else begin
        // Default outputs
        cpu_ready <= 0;
        
        // Count stall cycles
        if (cpu_stall && (state != IDLE || pending_request)) begin
            stall_cycles <= stall_cycles + 1;
        end
        
        case (state)
            IDLE: begin
                if (cpu_req && !cpu_stall) begin
                    if (hit) begin
                        // Cache hit
                        cpu_data <= cache_data[get_cache_index(set_index, hit_way, offset)];
                        cpu_ready <= 1;
                        hit_count <= hit_count + 1;
                        pending_request <= 0;
                        
                        // Update LRU
                        cache_lru[set_index] <= !hit_way;
                    end else begin
                        // Cache miss
                        word_cnt <= 0;
                        saved_addr <= cpu_addr;
                        saved_way <= victim_way;
                        mem_req <= 1;
                        mem_addr <= {cpu_addr[ADDR_WIDTH-1:BLOCK_OFFSET_BITS], {BLOCK_OFFSET_BITS{1'b0}}};
                        miss_count <= miss_count + 1;
                        pending_request <= 1;
                    end
                end else if (pending_request && !cpu_stall) begin
                    // Deliver pending data
                    cpu_data <= cache_data[get_cache_index(saved_set, saved_way, saved_offset)];
                    cpu_ready <= 1;
                    pending_request <= 0;
                end
            end
            
            FETCH: begin
                if (mem_ready) begin
                    // Store fetched data
                    cache_data[get_cache_index(saved_set, saved_way, word_cnt)] <= mem_data;
                    
                    // Update tag and valid on first word
                    if (word_cnt == 0) begin
                        cache_valid[saved_set][saved_way] <= 1;
                        cache_tag[saved_set][saved_way] <= saved_addr[ADDR_WIDTH-1:SET_BITS+BLOCK_OFFSET_BITS];
                    end
                    
                    // Check if this is the requested word
                    if (word_cnt == saved_offset && !cpu_stall) begin
                        cpu_data <= mem_data;
                        cpu_ready <= 1;
                        pending_request <= 0;
                        // Update LRU
                        cache_lru[saved_set] <= !saved_way;
                    end else if (word_cnt == saved_offset) begin
                        pending_request <= 1;
                    end
                    
                    // Continue fetching or finish
                    if (word_cnt == 3) begin
                        mem_req <= 0;
                        // Update LRU for miss
                        cache_lru[saved_set] <= !saved_way;
                    end else begin
                        word_cnt <= word_cnt + 1;
                        mem_addr <= {saved_addr[ADDR_WIDTH-1:BLOCK_OFFSET_BITS], {BLOCK_OFFSET_BITS{1'b0}}} + 
                                   ((word_cnt + 1) << 2);
                    end
                end
            end
            
            WAIT_PIPELINE: begin
                if (!cpu_stall && pending_request) begin
                    // Deliver pending data
                    cpu_data <= cache_data[get_cache_index(saved_set, saved_way, saved_offset)];
                    cpu_ready <= 1;
                    pending_request <= 0;
                end
            end
        endcase
    end
end

// Debug output
assign debug_info = debug_en ? {
    state,                   // [127:126]
    hit,                     // [125]
    hit_way0,                // [124]
    hit_way1,                // [123]
    victim_way,              // [122]
    cache_lru[set_index],    // [121]
    cpu_req,                 // [120]
    cpu_ready,               // [119]
    8'h00,                   // [118:111]
    set_index,               // [110:105]
    offset,                  // [104:103]
    tag[7:0],                // [102:95]
    31'h0                    // [94:64]
} : 128'h0;

endmodule
