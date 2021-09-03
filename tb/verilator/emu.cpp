/*
* @Author: Ruige Lee
* @Date:   2021-09-01 17:23:24
* @Last Modified by:   Ruige Lee
* @Last Modified time: 2021-09-01 18:52:53
*/




Emulator::Emulator(int argc, const char *argv[]):
  dut_ptr(new VSimTop),
  cycles(0), trapCode(STATE_RUNNING)
{
  args = parse_args(argc, argv);

  // srand
  srand(args.seed);
  srand48(args.seed);
  Verilated::randReset(2);
  assert_init();

  // init core
  reset_ncycles(10);

  // init ram
  init_ram(args.image);

#if VM_TRACE == 1

  enable_waveform = args.enable_waveform;
  if (enable_waveform) {
    Verilated::traceEverOn(true);	// Verilator must compute traced signals
    tfp = new VerilatedVcdC;
    dut_ptr->trace(tfp, 99);	// Trace 99 levels of hierarchy
    time_t now = time(NULL);
    tfp->open(waveform_filename(now));	// Open the dump file
  }

#endif

}

Emulator::~Emulator() {
  ram_finish();
  assert_finish();

}

inline void Emulator::reset_ncycles(size_t cycles) {
  for(int i = 0; i < cycles; i++) {
    dut_ptr->reset = 1;
    dut_ptr->clock = 0;
    dut_ptr->eval();
    dut_ptr->clock = 1;
    dut_ptr->eval();
    dut_ptr->reset = 0;
  }
}

inline void Emulator::single_cycle() {
  dut_ptr->clock = 0;
  dut_ptr->eval();

  dut_ptr->clock = 1;
  dut_ptr->eval();

#if VM_TRACE == 1
  if (enable_waveform) {
    auto trap = difftest[0]->get_trap_event();
    uint64_t cycle = trap->cycleCnt;
    uint64_t begin = dut_ptr->io_logCtrl_log_begin;
    uint64_t end   = dut_ptr->io_logCtrl_log_end;
    bool in_range = (begin <= cycle) && (cycle <= end);
    if (in_range) { tfp->dump(cycle); }
  }
#endif

  // if (dut_ptr->io_uart_out_valid) {
  //   printf("%c", dut_ptr->io_uart_out_ch);
  //   fflush(stdout);
  // }
  // if (dut_ptr->io_uart_in_valid) {
  //   extern uint8_t uart_getc();
  //   dut_ptr->io_uart_in_ch = uart_getc();
  // }
  cycles ++;
}

uint64_t Emulator::execute(uint64_t max_cycle, uint64_t max_instr) {
  uint32_t lasttime_poll = 0;
  uint32_t lasttime_snapshot = 0;
  // const int stuck_limit = 5000;
  // const int firstCommit_limit = 10000;
  uint64_t core_max_instr[EMU_CORES];
  for (int i = 0; i < EMU_CORES; i++) {
    core_max_instr[i] = max_instr;
  }

  uint32_t t = uptime();
  if (t - lasttime_poll > 100) {
    poll_event();
    lasttime_poll = t;
  }

// #ifdef EN_FORKWAIT
//   printf("[INFO]enable fork wait..\n");
//   pid_t pid =-1;
//   pid_t originPID = getpid();
//   int status = -1;
//   int slotCnt = 1;
//   int waitProcess = 0;
//   uint32_t timer = 0;
//   std::list<pid_t> pidSlot = {};
//   enable_waveform = false;

//   //first process as a control process
//   if((pid = fork()) < 0 ){
//     perror("First fork failed..\n");
//     FAIT_EXIT;
//   } else if(pid > 0) {  //parent process
//     printf("[%d] Control process first fork...child: %d\n ",getpid(),pid);
//     prctl(PR_SET_CHILD_SUBREAPER, 1, 0, 0, 0);
//     forkshm.shwait();
//     printf("[%d] Emulationg finished, Control process exit..",getpid());
//     return cycles;
//   } else {
//     forkshm.info->exitNum++;
//     forkshm.info->flag = true;
//     pidSlot.insert(pidSlot.begin(),  getpid());
//   }
// #endif

// #if VM_COVERAGE == 1
//   // we dump coverage into files at the end
//   // since we are not sure when an emu will stop
//   // we distinguish multiple dat files by emu start time
//   time_t coverage_start_time = time(NULL);
// #endif

  while (!Verilated::gotFinish() && trapCode == STATE_RUNNING) {
    // cycle limitation
    if (!max_cycle) {
      trapCode = STATE_LIMIT_EXCEEDED;
      break;
    }
    // instruction limitation
    for (int i = 0; i < EMU_CORES; i++) {
      if (!core_max_instr[i]) {
        trapCode = STATE_LIMIT_EXCEEDED;
        break;
      }
    }
    // assertions
    if (assert_count > 0) {
      // for (int i = 0;  )
      // difftest[0]->display();
      eprintf("The simulation stopped. There might be some assertion failed.\n");
      trapCode = STATE_ABORT;
      break;
    }
    // signals
    if (signal_num != 0) {
      trapCode = STATE_SIG;
    }
    if (trapCode != STATE_RUNNING) {
      break;
    }

    for (int i = 0; i < EMU_CORES; i++) {
      auto trap = difftest[i]->get_trap_event();
      if (trap->instrCnt >= args.warmup_instr) {
        printf("Warmup finished. The performance counters will be dumped and then reset.\n");
        dut_ptr->io_perfInfo_clean = 1;
        dut_ptr->io_perfInfo_dump = 1;
        args.warmup_instr = -1;
      }
      if (trap->cycleCnt % args.stat_cycles == args.stat_cycles - 1) {
        dut_ptr->io_perfInfo_clean = 1;
        dut_ptr->io_perfInfo_dump = 1;
      }
    }

    single_cycle();

    max_cycle --;
    dut_ptr->io_perfInfo_clean = 0;
    dut_ptr->io_perfInfo_dump = 0;

    // Naive instr cnt per core
    for (int i = 0; i < EMU_CORES; i++) {
      // update instr_cnt
      uint64_t commit_count = (core_max_instr[i] >= difftest[i]->num_commit) ? difftest[i]->num_commit : core_max_instr[i];
      core_max_instr[i] -= commit_count;
    }

    trapCode = difftest_state();
    if (trapCode != STATE_RUNNING) break;

    if (difftest_step()) {
      trapCode = STATE_ABORT;
      break;
    }
    if (trapCode != STATE_RUNNING) break;


// #ifdef EN_FORKWAIT  
//     timer = uptime();
//     if(timer - lasttime_snapshot > 1000 * FORK_INTERVAL && !waitProcess ){   //time out need to fork
//       lasttime_snapshot = timer;
//       if(slotCnt == SLOT_SIZE) {     //kill first wait process
//           pid_t temp = pidSlot.back();
//           pidSlot.pop_back();
//           kill(temp, SIGKILL); 
//           slotCnt--;
//           forkshm.info->exitNum--;
//       }
//       //fork-wait
//       if((pid = fork())<0){
//           eprintf("[%d]Error: could not fork process!\n",getpid());
//           return -1;
//       } else if(pid != 0) {       //father fork and wait.
//           waitProcess = 1;
//           wait(&status);
//           enable_waveform = forkshm.info->resInfo != STATE_GOODTRAP;
//           if (enable_waveform) {
//             Verilated::traceEverOn(true);	// Verilator must compute traced signals
//             tfp = new VerilatedVcdC;
//             dut_ptr->trace(tfp, 99);	// Trace 99 levels of hierarchy
//             time_t now = time(NULL);
//             tfp->open(waveform_filename(now));	// Open the dump file
//           }
//       } else {        //child insert its pid
//           slotCnt++;
//           forkshm.info->exitNum++;
//           pidSlot.insert(pidSlot.begin(),  getpid());
//       }
//     } 
// #endif
    }

#if VM_TRACE == 1
  if (enable_waveform) tfp->close();
#endif

// #if VM_COVERAGE == 1
//   save_coverage(coverage_start_time);
// #endif

// #ifdef EN_FORKWAIT
//   if(!waitProcess) display_trapinfo();
//   else printf("[%d] checkpoint process: dump wave complete, exit.\n",getpid());
//   forkshm.info->exitNum--;
//   forkshm.info->resInfo = trapCode;
// #endif

  display_trapinfo();

  return cycles;
}


inline char* Emulator::timestamp_filename(time_t t, char *buf) {
  char buf_time[64];
  strftime(buf_time, sizeof(buf_time), "%F@%T", localtime(&t));
  char *noop_home = getenv("NOOP_HOME");
  assert(noop_home != NULL);
  int len = snprintf(buf, 1024, "%s/build/%s", noop_home, buf_time);
  return buf + len;
}

// #ifdef VM_SAVABLE
// inline char* Emulator::snapshot_filename(time_t t) {
//   static char buf[1024];
//   char *p = timestamp_filename(t, buf);
//   strcpy(p, ".snapshot");
//   return buf;
// }
// #endif


inline char* Emulator::waveform_filename(time_t t) {
  static char buf[1024];
  char *p = timestamp_filename(t, buf);
  strcpy(p, ".vcd");
  printf("dump wave to %s...\n", buf);
  return buf;
}


// #if VM_COVERAGE == 1
// inline char* Emulator::coverage_filename(time_t t) {
//   static char buf[1024];
//   char *p = timestamp_filename(t, buf);
//   strcpy(p, ".coverage.dat");
//   return buf;
// }

// inline void Emulator::save_coverage(time_t t) {
//   char *p = coverage_filename(t);
//   VerilatedCov::write(p);
// }
// #endif

void Emulator::trigger_stat_dump() {
  dut_ptr->io_perfInfo_dump = 1;
  if(get_args().force_dump_result) {
    dut_ptr->io_logCtrl_log_end = -1;
  }
  single_cycle();
}

void Emulator::display_trapinfo() {
  for (int i = 0; i < EMU_CORES; i++) {
    printf("Core %d: ", i);
    auto trap = difftest[i]->get_trap_event();
    uint64_t pc = trap->pc;
    uint64_t instrCnt = trap->instrCnt;
    uint64_t cycleCnt = trap->cycleCnt;

    switch (trapCode) {
      case STATE_GOODTRAP:
        eprintf(ANSI_COLOR_GREEN "HIT GOOD TRAP at pc = 0x%" PRIx64 "\n" ANSI_COLOR_RESET, pc);
        break;
      case STATE_BADTRAP:
        eprintf(ANSI_COLOR_RED "HIT BAD TRAP at pc = 0x%" PRIx64 "\n" ANSI_COLOR_RESET, pc);
        break;
      case STATE_ABORT:
        eprintf(ANSI_COLOR_RED "ABORT at pc = 0x%" PRIx64 "\n" ANSI_COLOR_RESET, pc);
        break;
      case STATE_LIMIT_EXCEEDED:
        eprintf(ANSI_COLOR_YELLOW "EXCEEDING CYCLE/INSTR LIMIT at pc = 0x%" PRIx64 "\n" ANSI_COLOR_RESET, pc);
        break;
      case STATE_SIG:
        eprintf(ANSI_COLOR_YELLOW "SOME SIGNAL STOPS THE PROGRAM at pc = 0x%" PRIx64 "\n" ANSI_COLOR_RESET, pc);
        break;
      default:
        eprintf(ANSI_COLOR_RED "Unknown trap code: %d\n", trapCode);
    }

    double ipc = (double)instrCnt / cycleCnt;
    eprintf(ANSI_COLOR_MAGENTA "total guest instructions = %'" PRIu64 "\n" ANSI_COLOR_RESET, instrCnt);
    eprintf(ANSI_COLOR_MAGENTA "instrCnt = %'" PRIu64 ", cycleCnt = %'" PRIu64 ", IPC = %lf\n" ANSI_COLOR_RESET,
        instrCnt, cycleCnt, ipc);
  }

  if (trapCode != STATE_ABORT) {
    trigger_stat_dump();
  }
}

// #ifdef EN_FORKWAIT
// ForkShareMemory::ForkShareMemory() {
//   if((key_n = ftok(".",'s')<0)) {
//       perror("Fail to ftok\n");
//       FAIT_EXIT
//   }
//   printf("key num:%d\n",key_n);

//   if((shm_id = shmget(key_n,1024,0666|IPC_CREAT))==-1) {
//       perror("shmget failed...\n");
//       FAIT_EXIT
//   }
//   printf("share memory id:%d\n",shm_id);

//   if((info = (shinfo*)(shmat(shm_id, NULL, 0))) == NULL ) {
//       perror("shmat failed...\n");
//       FAIT_EXIT
//   }

//   info->exitNum   = 0;
//   info->flag      = false;
//   info->resInfo   = -1;           //STATE_RUNNING
// }

// ForkShareMemory::~ForkShareMemory() {
//   if(shmdt(info) == -1 ){
//     perror("detach error\n");
//   }
//   shmctl(shm_id, IPC_RMID, NULL) ;
// }

// void ForkShareMemory::shwait(){
//     while(true){
//         if(info->exitNum == 0 && info->flag){ break;  } 
//         else {  
//             sleep(WAIT_INTERVAL);  
//         }
//     }
// }
// #endif


