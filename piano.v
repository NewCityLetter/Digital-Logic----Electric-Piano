module Divider #(parameter N=100000)
(
   input  I_CLK,
   output reg O_CLK_1M
);
        integer  i=0;
        always@(posedge I_CLK)
        begin
            if(i==N/2-1) 
            begin
               O_CLK_1M<=~O_CLK_1M;
               i<=0;
            end
            else
               i<=i+1;
        end         
   endmodule 
   
module Adjust_vol(
    input clk,
    input [3:0] hex1,
    input [3:0] hex0,
    input keyup,
    output reg [3:0] vol_class=10,
    output reg [15:0]vol=16'h0000
    );
    wire [15:0]adjusted_vol;
    assign adjusted_vol=vol;
    integer clk_cnt=0;
    always @(posedge clk) 
    begin
        if(clk_cnt==200000) 
        begin
            clk_cnt<=0;
            if(!keyup)
            begin
            case({hex1,hex0})
            8'b01110101:
            begin
                vol<=(vol==16'h0000)?16'h0000:(vol-16'h197f);
            end
            8'b01110010:
            begin
                vol<=(vol==16'hfef6)?16'hfefe:(vol+16'h197f);
            end
            endcase
            end
        end
        else 
            clk_cnt<=clk_cnt+1;
    end
    always @(posedge clk)
    begin
        case(vol)
        16'he577:
        begin
            vol_class<=0;
        end
        16'hcbf8:
        begin
            vol_class<=1;
        end
        16'hb279:
        begin
            vol_class<=2;
        end
        16'h98fa:
        begin
            vol_class<=3;
        end
        16'h7f7b:
        begin
            vol_class<=4;
        end
        16'h65fc:
        begin
            vol_class<=5;
        end
        16'h4c7d:
        begin
            vol_class<=6;
        end
        16'h32fe:
        begin
            vol_class<=7;
        end
        16'h197f:
        begin
            vol_class<=8;
        end
        16'h0000:
        begin
            vol_class<=9;    
        end
        default:
        begin
            vol_class<=0;
        end
        endcase 
    end
endmodule 

module display7(rst,iData,oData);
    input rst;
    input [3:0] iData;
    output reg [6:0] oData;
    always@(*)
        begin
            case({rst,iData})
            5'b10000:oData=7'b1000000;
            5'b10001:oData=7'b1111001;
            5'b10010:oData=7'b0100100;
            5'b10011:oData=7'b0110000;
            5'b10100:oData=7'b0011001;
            5'b10101:oData=7'b0010010;
            5'b10110:oData=7'b0000010;
            5'b10111:oData=7'b1111000;
            5'b11000:oData=7'b0000000;
            5'b11001:oData=7'b0010000;
            default:oData=7'b1111111;
            endcase
        end
endmodule


//实现了一个移位寄存器来保存22位数据，
//根据移位寄存器的某些位输出两个十六进制值，
//从KB移入SDATA并在读取" F0"时发出选通信号
module KBDecoder(
    input CLK,//同步后usb信号
    input SDATA,//同步后usb数据
    input ARST_L,//控制信号
    output [3:0] HEX0,//16进制键码
    output [3:0] HEX1,//16进制键码
    output reg KEYUP//返回是否读取到键的信息，1为读取到
    );
    
wire arst_i, rollover_i;
reg [21:0] Shift;

assign arst_i = ~ARST_L;
// 从移位寄存器中不停取值
assign HEX0[3:0] = Shift[15:12];
assign HEX1[3:0] = Shift[19:16];

// 移入一次需要22个同步后的时钟周期（按下与放开各11个，信息为FOXX,XX为按下键的键码）
always @(negedge CLK or posedge arst_i) begin;
    if(arst_i)begin
        Shift <= 22'b0000000000000000000000;
    end
    else begin
        Shift <= {SDATA, Shift[21:1]}; //从最后一位移入
    end
end

//找到F0即代表找到了一个码，发送一个1的值
always @(posedge CLK) begin
    if(Shift[8:1] == 8'hF0) begin
        KEYUP <= 1'b1;
    end
    else begin
        KEYUP <= 1'b0;
    end    
end    

endmodule

module MP3(
	input CLK,//系统时钟
	input CLK_1M,
	input DREQ,//数据请求
	output reg XRSET,//硬件复位
	output reg XCS,//低电平有效片选输兿
	output reg XDCS,////数据片字节同步
	output reg SI,//串行数据输入	
	output reg SCLK,//SPI时钟
	input init,//初始化
	input [3:0] hex1,//16进制键码高位
    input [3:0] hex0,//16进制键码低位
    input [15:0] adjusted_vol,
    input keyup,
    output reg [3:0] tune=0
    
);
    parameter  CMD_START=0;//弿始写指令
    parameter  WRITE_CMD=1;//将一条指令全部写兿
    parameter  DATA_START=2;//弿始写数据
    parameter  WRITE_DATA=3;//将一条数据全部写兿
    parameter  DELAY=4;//延时  
    parameter VOL_CMD_START=5;// 音量相关
    parameter SEND_VOL_CMD=6;

	reg [31:0] volcmd;
  
    reg [20:0]addr;
    
    reg  [15:0] Data;
    wire [15:0] D_do;
    wire [15:0] D_re;
    wire [15:0] D_mi;
    wire [15:0] D_fa;
    wire [15:0] D_so;
    wire [15:0] D_la;
    wire [15:0] D_xi;
    wire [15:0] D_hdo;
    wire [15:0] D_hre;
    wire [15:0] D_hmi;
    wire [15:0] music1;
    wire [15:0] music2;
    wire [15:0] music3;
    wire [15:0] music4;
    
    reg [3:0] pretune=0;  
    reg [15:0] _Data;
    blk_mem_gen_1 your_instance_name1(.clka(CLK),.ena(1),.addra(addr),.douta(D_do));
    blk_mem_gen_2 your_instance_name2(.clka(CLK),.ena(1),.addra(addr),.douta(D_re));
    blk_mem_gen_3 your_instance_name3(.clka(CLK),.ena(1),.addra(addr),.douta(D_mi));
    blk_mem_gen_4 your_instance_name4(.clka(CLK),.ena(1),.addra(addr),.douta(D_fa));
    blk_mem_gen_5 your_instance_name5(.clka(CLK),.ena(1),.addra(addr),.douta(D_so));
    blk_mem_gen_6 your_instance_name6(.clka(CLK),.ena(1),.addra(addr),.douta(D_la));
    blk_mem_gen_7 your_instance_name7(.clka(CLK),.ena(1),.addra(addr),.douta(D_xi));
    blk_mem_gen_8 your_instance_name8(.clka(CLK),.ena(1),.addra(addr),.douta(D_hdo));
    blk_mem_gen_9 your_instance_name9(.clka(CLK),.ena(1),.addra(addr),.douta(D_hre));
    blk_mem_gen_10 your_instance_name10(.clka(CLK),.ena(1),.addra(addr),.douta(D_hmi));
    blk_mem_gen_11 your_instance_name11(.clka(CLK),.ena(1),.addra(addr),.douta(music1));//year
    blk_mem_gen_12 your_instance_name12(.clka(CLK),.ena(1),.addra(addr),.douta(music2));//mojito
    blk_mem_gen_13 your_instance_name13(.clka(CLK),.ena(1),.addra(addr),.douta(music3));//north
    blk_mem_gen_14 your_instance_name14(.clka(CLK),.ena(1),.addra(addr),.douta(music4));//balloon
   
    integer tune_delay=0;
	always @(posedge CLK_1M)
	begin
	   if(tune_delay==0) 
	   begin
            if(keyup) 
            begin
               tune_delay<=50000;
               case({hex1,hex0})
               8'b00010110:
               begin
                   tune<=4'b0001;
               end
               8'b00011110:
               begin
                    tune<=4'b0010;
               end
               8'b00100110:
               begin
                    tune<=4'b0011;
               end
               8'b00100101:
               begin
                    tune<=4'b0100;
               end
               8'b00101110:
               begin
                    tune<=4'b0101;
               end
               8'b00110110:
               begin
                    tune<=4'b0110;
               end
               8'b00111101:
               begin
                    tune<=4'b0111;
               end
               8'b00111110:
               begin
                    tune<=4'b1000;
               end
               8'b01000110:
               begin
                    tune<=4'b1001;
               end
               8'b01000101:
               begin
                    tune<=4'b1010;
               end
               8'b01001110:
               begin
                    tune<=4'b1011;
               end
               8'b01010101:
               begin
                    tune<=4'b1100;
               end
               8'b01010100:
               begin
                    tune<=4'b1101;
               end
               8'b01011011:
               begin
                    tune<=4'b1110;
               end
               default:
               begin
                    //tune<=0;
               end
               endcase
             end
       end
       else 
       begin
            tune_delay<=tune_delay-1;
       end
	   
	   
	   case(tune)
	   4'b0001:
	   begin
	       Data<=D_do;
	   end
	   4'b0010:
	   begin
	       Data<=D_re;
	   end
	   4'b0011:
	   begin
	       Data<=D_mi;
	   end
	   4'b0100:
	   begin
	       Data<=D_fa;
	   end
	   4'b0101:
	   begin
	       Data<=D_so;
	   end
	   4'b0110:
	   begin
	       Data<=D_la;
	   end
	   4'b0111:
	   begin
	       Data<=D_xi;
	   end
	   4'b1000:
	   begin
	       Data<=D_hdo;
	   end
	   4'b1001:
	   begin
	       Data<=D_hre;
	   end
	   4'b1010:
	   begin
	       Data<=D_hmi;
	   end
	   4'b1011:
	   begin
	       Data<=music1;
	   end
	   4'b1100:
	   begin
	       Data<=music2;
	   end
	   4'b1101:
	   begin
	       Data<=music3;
	   end
	   4'b1110:
	   begin
	       Data<=music4;
	   end
	   default:
	   begin
	       //Data<=D_none;
	   end
	   endcase
	end
	
	reg [63:0] cmd={32'h02000804,32'h020B0000};//00是控制模式 0B是音量 08 本地模式 04 软件复位（每首歌之后软件复位）
    integer status=CMD_START;
    integer cnt=0;//位计敿
    integer cmd_cnt=0;//命令计数
	
    always @(posedge CLK_1M) 
	begin
	    pretune<=tune;
        if(~init||pretune!=tune||!keyup) 
	    begin
            XCS<=1;
            XDCS<=1;
            XRSET<=0;
            cmd_cnt<=0;
            status<=DELAY;  // 刚开机时先delay,等待DREQ
            SCLK<=0;
            cnt<=0;
            addr<=0;
        end
        
        else if((tune<4'b1011&&addr<10000)||(tune>4'b1010))
	    begin
            case(status)
            CMD_START: // 等待允许输入
		    begin
                SCLK<=0;//时钟下降沿输入，上升沿读取
                if(cmd_cnt>=2) // 把前2组预设命令（mode 音量 ）输入完毕后 开始输入音调
					status<=DATA_START;
                else if(DREQ) // DREQ有效旿 允许输入 si可以接受32（bit）信号
                begin  
                    XCS<=0;//XCS拉低表示输入指令
                    status<=WRITE_CMD;  // 开始输入
                    SI<=cmd[63];
                    cmd<={cmd[62:0],cmd[63]}; 
                    cnt<=1;
                end
            end
            WRITE_CMD://写入指令
            begin
                if(DREQ) 
                begin
                    if(SCLK) 
                    begin
                        if(cnt>=32)
                        begin
                            XCS<=1;  // 取消复位
                            cnt<=0;
                            cmd_cnt<=cmd_cnt+1;
                            status<=CMD_START;  // 跳转到命令执行
                        end
                        else 
                        begin
                            SCLK<=0;
                           SI<=cmd[63]; // 发送三十二位写指令（写指令0200（择MODE寄存器） 0804（MODE有十六位 这里只关心第11和2位 进行软复位）
                           cmd<={cmd[62:0],cmd[63]}; //循环传送
                           cnt<=cnt+1; 
                        end
                    end
                    SCLK<=~SCLK;
                end
            end
            
            DATA_START://写入数据
            begin
                if(adjusted_vol[15:0]!=cmd[15:0])  // cmd[47:32] 里存储的是当前音量 初始值为0000
                begin//音量变了
                    cnt<=0;
                    volcmd<={16'h020B,adjusted_vol}; // 调节音量命令 0B寄存器负责音量控刿  该寄存器存储内容表示音量大小
                    status<=VOL_CMD_START;			// 转到音量调节
                end
                else if(DREQ) // 等待允许输入 之后每次输入32使 等DREQ下次变高接着输入 vs1003B会自动接收并播放
                begin
                    XDCS<=0;
                    SCLK<=0;
                    SI<=Data[15];
                    _Data<={Data[14:0],Data[15]};
                    cnt<=1;    
                    status<=WRITE_DATA;  // 歌曲信号
                end
                cmd[15:0]<=adjusted_vol; // 更新cmd中存储的音量
            end
            
            WRITE_DATA:
            begin  
                if(SCLK)
                begin
                    if(cnt>=16)
                    begin
                        XDCS<=1;
                        addr<=addr+1; // 读完十六位后地址后移1位
                        status<=DATA_START;
                    end
                    else 
                    begin  // 循环输入 输入十六位
                        SCLK<=0;
                        cnt<=cnt+1;
                        _Data<={_Data[14:0],_Data[15]};
                        SI<=_Data[15];
                    end
                end
                SCLK<=~SCLK;
            end
          
            DELAY:
            begin
                if(cnt<50000)   // 等待100个时钟周朿
                    cnt<=cnt+1;
                else 
                begin
                    cnt<=0;
                    status<=CMD_START;  // 弿始输入命仿
                    XRSET<=1;
                end
            end
            
            VOL_CMD_START:
            begin
                if(DREQ) 
                begin  // 等待DREQ信号
                    XCS<=0;  // 低电平有效片选输兿
                    status<=SEND_VOL_CMD;   // 输入音量控制命令
                    SI<=volcmd[31];
                    volcmd<={volcmd[30:0],volcmd[31]}; 
                    cnt<=1;
                end
            end
            
            SEND_VOL_CMD:
            begin
                if(DREQ) 
                begin
                     if(SCLK) 
                     begin
                        if(cnt<32)//循环传值
                        begin
                            SI<=volcmd[31];
                            volcmd<={volcmd[30:0],volcmd[31]}; 
                            cnt<=cnt+1; 
                        end
                        else 
                        begin 
                            XCS<=1; // 结束输入
                            cnt<=0;
                            status<=DATA_START; // 继续之前的输兿
                          
                        end
                    end
                    SCLK<=~SCLK;
                end
            end
		default:;
        endcase
    end
end
endmodule

module oled(
	input CLK, 
	input RST,
	input [3: 0] current,
	output reg DIN, // input pin 
	output reg OLED_CLK, 
	output reg CS, // chip select
	output reg DC, // Data & CMD 
	output reg RES
);
	parameter DELAY_TIME = 25000;
	
	// DC parameter
	parameter CMD = 1'b0;
	parameter DATA = 1'b1;
	
	// init cmds
	reg [383:0] cmds;
	initial
		begin
			cmds= {
			8'hAE, 8'hA0, 8'h76, 8'hA1, 8'h00, 8'hA2,
			8'h00, 8'hA4, 8'hA8, 8'h3F, 8'hAD, 8'h8E, 
			8'hB0, 8'h0B, 8'hB1, 8'h31, 8'hB3, 8'hF0,
			8'h8A, 8'h64, 8'h8B, 8'h78, 8'h8C, 8'h64,
			8'hBB, 8'h3A, 8'hBE, 8'h3E, 8'h87, 8'h06,
			8'h81, 8'h91, 8'h82, 8'h50, 8'h83, 8'h7D, 
			8'h15, 8'h00, 8'h5F, 8'h75, 8'h00, 8'h3F, 
			8'haf, 8'h00, 8'h00, 8'h00, 8'h00, 8'h00}; 
		end
 
	// base map 
	wire [1535:0] map;
	reg [5: 0] addr;
	blk_mem_gen_0 your_instance_name(.clka(CLK),.ena(1),.addra({current, addr}),.douta(map));
	
	// states
	parameter CMD_PRE = 0;
	parameter PRE_WRITE = 1;
	parameter WRITE = 2;
	parameter DATA_PRE = 3;
	
	wire clk_div;
    Divider #(.N(50)) CLKDIV2(CLK, clk_div);
	
	// vars 
	reg [1535:0] temp;
	reg [15: 0] cmd_cnt;
	reg [7: 0] data_reg;
	reg [3: 0] state;
	reg [3: 0] state_pre;
	integer cnt = 0;
	integer write_cnt = 0;
	
	// state machine
	always @ (posedge clk_div) begin 
		if(!RST) begin 
			state <= CMD_PRE;
			cmd_cnt <= 0;
			CS <= 1'b1;
			RES <= 0;
		end
		else begin 
			RES <= 1;
			case(state)
				// prepare for cmd write, put cmds rows into temp 
				CMD_PRE: 
				begin 
						if(cmd_cnt == 1) 
						begin 
							cmd_cnt <= 0;
							addr <= 0;
							state <= DATA_PRE;
						end
						else begin 
							temp <= cmds;
							state <= PRE_WRITE;
							state_pre <= CMD_PRE;
							write_cnt <= 48;
							DC <= CMD;
						end
					end
				// prepare for data write 
				DATA_PRE: 
				begin 
						if(cmd_cnt == 64) 
						begin 
							cmd_cnt <= 0;
							state <= DATA_PRE;
						end
						else 
						begin 
							temp <= map;
							state <= PRE_WRITE;
							state_pre <= DATA_PRE;
							write_cnt <= 192;
							DC <= DATA;
						end
					end
				// cut temp into several 8bits regs
				PRE_WRITE: 
				begin 
						if(write_cnt == 0) 
						begin 
							cmd_cnt <= cmd_cnt+1;
							addr <= addr+1;
							state <= state_pre;
						end
						else 
						begin 
							data_reg[7: 0] <= (state_pre==CMD_PRE)? temp[383: 376]: temp[1535: 1528];
							temp <= (state_pre==CMD_PRE)? {temp[375: 0], temp[383: 376]}: {temp[1527: 0], temp[1535: 1528]};
							state <= WRITE;
							OLED_CLK <= 0;
							cnt <= 0;
						end
					end
				// shift 8bits into DIN port
				WRITE: 
				begin 
						if(OLED_CLK) 
						begin 
							if(cnt == 8) 
							begin 
								CS <= 1;
								write_cnt <= write_cnt-1;
								state <= PRE_WRITE;
							end
							else 
							begin 
								CS <= 0;
								DIN <= data_reg[7];
								cnt <= cnt+1;
								data_reg<={data_reg[6:0], data_reg[7]}; 
							end
						end
						OLED_CLK <= ~OLED_CLK;
					end
				default:;
			endcase
		end
	end 
endmodule


module top(
	input CLK,//系统时钟
	input DREQ,//数据请求
	output wire XRSET,//硬件复位
	output wire XCS,//低电平有效片选输兿
	output wire XDCS,////数据片字节同步
	output wire SI,//串行数据输入	
	output wire SCLK,//SPI时钟
	output wire [6:0] oData,
	input init,//初始化
	input usbCLK,//usb时钟 F4
    input usbDATA,//usb数据 B2
	output wire [3:0] hex1,//16进制键码高位
    output wire [3:0] hex0,//16进制键码低位
    
    output wire DIN, // input pin 
    output wire OLED_CLK, 
    output wire CS, // chip select
    output wire DC, // Data & CMD 
    output wire RES
);
    wire  usbclk,usbdata,keyup;//同步后的usb信号，同步后的usb数据，是否读取到键的信息（1为读取到）
    wire [15:0] adjusted_vol;
	wire [3:0] vol_class;
	wire [3:0] tune;
	
    KBDecoder keyboard(.CLK(usbCLK), .SDATA(usbDATA), .ARST_L(init), .HEX1(hex1), .HEX0(hex0), .KEYUP(keyup));
    Divider #(.N(100)) CLKDIV1(CLK,CLK_1M);
	Adjust_vol adjvol(.clk(CLK_1M), .hex1(hex1), .hex0(hex0), .keyup(keyup),.vol_class(vol_class),.vol(adjusted_vol));  //output
    oled OLED(.CLK(CLK),.RST(init),.current(tune),.DIN(DIN),.OLED_CLK(OLED_CLK),.CS(CS),.DC(DC),.RES(RES));
	display7 Display7(.rst(init),.iData(vol_class),.oData(oData));
	MP3 mp3(.CLK(CLK),.CLK_1M(CLK_1M),.DREQ(DREQ),.XRSET(XRSET),.XCS(XCS),.XDCS(XDCS),.SI(SI),.SCLK(SCLK),.init(init),.hex1(hex1),.hex0(hex0),.adjusted_vol(adjusted_vol),.keyup(keyup),.tune(tune));
endmodule