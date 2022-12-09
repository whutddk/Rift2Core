# Configuration

You can compile a core according to a new configuration, [Rift2GoCfg](../src/test/scala/rift2Core/rift2Chip_tb.scala#L40) is an example for Minimal area configuration.

----------------------------------------


## usage

All available configurations can be found at [Parameter.scala](../src/main/scala/Parameters.scala)


* `hasFpu`: Whether the FPU is instanced, or whether RV64FD is supported
* `hasPreFetch`: Whether a real PreFetcher is instanced

* `isMinArea`: Try our best to reduce the area, the entries of most of the buff, queue, FIFO will reduce to minimal
* `isLowPower`: If `isLowPower` is set to `true`, the `bits` of DecoupleIO will be forced to `0.U` instead of `DontCare` when `valid` is `false.B`

* `rnChn`: How many channels for renaming
* `cmChn`: How many channels for committing
* `opChn`: How many channels for reading operators
* `wbChn`: How many channels for writing-back results

* `xRegNum`: The number of physical register files of XPU, should be larger than 32 for renaming
* `fRegNum`: The number of physical register files of FPU, should be larger than 32 for renaming
* `vRegNum`: The number of physical register files ov VPU, should be larger than 32 for renaming
* `pmpNum`: The number of physical memory protection groups
* `hpmNum`: The number of performance monitor registers

* `l1BeatBits`: The data Width of L1 Bus
* `memBeatBits`: The data Width of L2 Memory Port

* `vlen`: The length of virtual address
* `plen`: The length of physical address

* `tlbEntry`: The number of tlb entries

* `uBTB_entry`: The number of uBTB entries
* `uBTB_tag_w`: The bits width of uBTB tags
* `btb_cl`: The number of BTB cache lines
* `bim_cl`: The number of BIM cache lines
* `ras_dp`: The depth of RAS
* `tage_table`: The number of TAGE tables (Do not support now)

* `dw`: The data width of I/D Cache
* `bk`: The number of Bank of I/D Cache
* `cb`: The number of Cache Block of I/D Cache
* `cl`: The number of Cache Line of I/D Cache
* `sbEntry`: The number of ScoreBoarc Entries of DCache
* `stEntry`: The number of StoreQueue Entries of DCache

-----------------------------------

## Attention

The configurations are not fully tested.
* Compile, test, and report the bugs
* Use the [NormalCfg](../src/test/scala/rift2Core/rift2Chip_tb.scala#L36) for full feature
* Use the [Rift2GoCfg](../src/test/scala/rift2Core/rift2Chip_tb.scala#L40) for minimal area

