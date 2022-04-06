import targets
import testlib
import os

class rift_hart(targets.Hart):
    xlen = 64
    ram = 0x80000000
    ram_size = 0x10000000

    reset_vectors = [0x1000]
    link_script_path = "rift.lds"
    misa = 0x800000000014112d


    # Path to linker script relative to the .py file where the target is
    # defined. Defaults to <name>.lds.
    link_script_path = None

    # Implements dmode in tdata1 as described in the spec. Harts that need
    # this value set to False are not compliant with the spec (but still usable
    # as long as running code doesn't try to mess with triggers set by an
    # external debugger).
    honors_tdata1_hmode = True


    # Address where we expect memory accesses to fail, usually because there is
    # no device mapped to that location.
    bad_address = None

    # Number of instruction triggers the hart supports.
    instruction_hardware_breakpoint_count = 0

    # Defaults to target-<index>
    name = None

    # When reset, the PC must be at one of the values listed here.
    # This is a list because on some boards the reset vector depends on
    # jumpers.
    reset_vectors = [0x80000000]

    # system is set to an identifier of the system this hart belongs to.  Harts
    # within the same system are assumed to share memory, and to have unique
    # hartids within that system.  So for most cases the default value of None
    # is fine.
    system = None





class rift(targets.Target):
    harts = [rift_hart]


    # Name of the target. Defaults to the name of the class.
    name = None

    # GDB remotetimeout setting.
    timeout_sec = 200

    # Timeout waiting for the server to start up. This is different than the
    # GDB timeout, which is how long GDB waits for commands to execute.
    # The server_timeout is how long this script waits for the server to be
    # ready for GDB connections.
    server_timeout_sec = 600

    # Path to OpenOCD configuration file relative to the .py file where the
    # target is defined. Defaults to <name>.cfg.
    openocd_config_path = "./test.cfg"

    # List of commands that should be executed in gdb after connecting but
    # before starting the test.
    gdb_setup = []

    # Supports mtime at 0x2004000
    supports_clint_mtime = False

    # Implements custom debug registers like spike does. It seems unlikely any
    # hardware will every do that.
    implements_custom_test = False

    # When true it indicates that reading invalid memory doesn't return an error
    invalid_memory_returns_zero = False

    # Supports simultaneous resume through hasel.
    support_hasel = False

    # Tests whose names are mentioned in this list will be skipped and marked
    # as not applicable. This is a crude mechanism that can be handy, but in
    # general it's better to define some property like those above that
    # describe behavior of this target, and tests can use that to decide
    # whether they are applicable or not.
    skip_tests = []

    # Set False if semihosting should not be tested in this configuration,
    # because it doesn't work and isn't expected to work.
    test_semihosting = True

    # Set False if manual hwbps (breakpoints set by directly writing tdata*)
    # isn't supposed to work.
    support_manual_hwbp = False

    # Set False if memory sampling is not supported due to OpenOCD
    # limitation/hardware support.
    support_memory_sampling = True

    # Relative path to a FreeRTOS binary compiled from the spike demo project
    # in https://github.com/FreeRTOS/FreeRTOS.
    freertos_binary = None

    # Internal variables:
    directory = None
    temporary_files = []







    def create(self):
        # 64-bit FPRs on 32-bit target
        # return os.system("$R2/tb/build/VSimTop &")
        return 0
