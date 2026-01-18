#include "Vtestbench.h"
#include "verilated.h"
#include "verilated_vcd_c.h"

int main(int argc, char** argv) {
    // 初始化 Verilator 环境
    Verilated::commandArgs(argc, argv);
    Verilated::debug(0);
    Verilated::randReset(0);
    Verilated::traceEverOn(true);

    // 创建 testbench 实例（testbench 内部自己生成时钟/复位）
    Vtestbench* top = new Vtestbench;

    // 可选：启用 VCD 波形导出
    VerilatedVcdC* tfp = new VerilatedVcdC;
    top->trace(tfp, 99);
    tfp->open("waveform.vcd");

    // 仿真主循环：仅执行 eval()，让 testbench 自己处理时钟/复位/结束逻辑
    uint64_t sim_cycles = 0;
    const uint64_t MAX_CYCLES = 300000000; // 最大仿真周期，避免死循环
    while (!Verilated::gotFinish() && sim_cycles < MAX_CYCLES) {
        // 执行一次仿真周期（testbench 内部的 forever #1 clk=!clk 会被 --no-timing 忽略，但逻辑仍执行）
        top->eval();

        // 导出波形（每周期递增 1ns）
        tfp->dump(sim_cycles);
        sim_cycles++;
    }

    // 清理资源
    tfp->close();
    delete tfp;
    delete top;

    // 正常退出
    return 0;
}