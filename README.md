# Rift2Core

![License](https://img.shields.io/badge/license-Apache-blue.svg)

[![LICENSE](https://img.shields.io/badge/license-Anti%20996-blue.svg)](https://github.com/996icu/996.ICU/blob/master/LICENSE)


--------------------------------------------


Based on Chisel3, Rift2Core is a 9-stage, dual-issue, out-of-order, 64-bits RISC-V Core, which supports RV64GC and M, S, U mode.

[RiftCore](https://github.com/whutddk/RiftCore) is the previous version of Rift2Core in Verilog.









----------------


## [How to Setup](doc/Setup.md)
* Setup Repo
* Setup sbt
* Setup verilator and gtkwave
* Compile chisel3 to verilog
* Compile Model of Rif2Chip
* Test a single ISA with waveform
* Test all ISA without waveform

## [How to Config](doc/Configuration.md)

----------------------


## Rift To Go

Download Pre-compile Version Here:


|Version|FIRRTL|Verilog|Test|Dhrystone|CoreMark|Area|
|:----: |:----:|:-----:|:--:|:-------:|:------:|:--:|
|Rift-2300|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2300/Rift2Chip.fir) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2300/Rift2Chip.fir)|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2300/Rift2Chip.v) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2300/Rift2Chip.v)|N/A|N/A|N/A|N/A|
|Rift-2310|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2310/Rift2Chip.fir) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2310/Rift2Chip.fir)|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2310/Rift2Chip.v) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2310/Rift2Chip.v) |N/A|N/A|N/A|N/A|
|Rift-2320|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2320/Rift2Chip.fir) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2320/Rift2Chip.fir)|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2320/Rift2Chip.v) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2320/Rift2Chip.v) |N/A|N/A|N/A|N/A|
|Rift-2330|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2330/Rift2Chip.fir) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2330/Rift2Chip.fir)|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2330/Rift2Chip.v) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2330/Rift2Chip.v) |![](https://img.shields.io/github/workflow/status/whutddk/rift2core/rift2330CI?label=)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2330/dhrystone.json)|![](https://img.shields.io/github/workflow/status/whutddk/rift2core/rift2330CI?label=)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2330/coremark.json)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2330/area.json)|
|Rift-2340|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2340/Rift2Chip.fir) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2340/Rift2Chip.fir)|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2340/Rift2Chip.v) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2340/Rift2Chip.v) |![](https://img.shields.io/github/workflow/status/whutddk/rift2core/rift2340CI?label=)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2340/dhrystone.json)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2340/coremark.json)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2340/area.json)|
|Rift-2350|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2350/Rift2Chip.fir) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2350/Rift2Chip.fir)|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2350/Rift2Chip.v) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2350/Rift2Chip.v) |![](https://img.shields.io/github/workflow/status/whutddk/rift2core/rift2350CI?label=)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2350/dhrystone.json)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2350/coremark.json)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2350/area.json)|
|Rift-2360|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2360/Rift2Chip.fir) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2360/Rift2Chip.fir)|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2360/Rift2Chip.v) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2360/Rift2Chip.v) |![](https://img.shields.io/github/workflow/status/whutddk/rift2core/rift2360CI?label=)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2360/dhrystone.json)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2360/coremark.json)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2360/area.json)|
|Rift-2370|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2370/Rift2Chip.fir) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2370/Rift2Chip.fir)|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2370/Rift2Chip.v) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2370/Rift2Chip.v) |![](https://img.shields.io/github/workflow/status/whutddk/rift2core/rift2370CI?label=)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2370/dhrystone.json)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2370/coremark.json)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2370/area.json)|
|Rift-2380|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2380/Rift2Chip.fir) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2380/Rift2Chip.fir)|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2380/Rift2Chip.v) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2380/Rift2Chip.v) |![](https://img.shields.io/github/workflow/status/whutddk/rift2core/rift2380CI?label=)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2380/dhrystone.json)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2380/coremark.json)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2380/area.json)|
|Rift-2390|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2390/Rift2Chip.fir) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2390/Rift2Chip.fir)|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2390/Rift2Chip.v) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2390/Rift2Chip.v) |![](https://img.shields.io/github/workflow/status/whutddk/rift2core/rift2390CI?label=)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2390/dhrystone.json)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2390/coremark.json)|![](https://img.shields.io/endpoint?url=https://raw.githubusercontent.com/whutddk/Rift2Core/gh-pages/shields/master/Rift2390/area.json)|





<!-- |Version|FIRRTL|Verilog|Test|Dhrystone|CoreMark|Area|
|:----: |:----:|:-----:|:--:|:-----:|:-----:|
|Rift-2300|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2300/Rift2Chip.fir) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2300/Rift2Chip.fir)|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2300/Rift2Chip.v) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2300/Rift2Chip.v)|N/A|N/A|N/A|
|Rift-2310|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2310/Rift2Chip.fir) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2310/Rift2Chip.fir)|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2310/Rift2Chip.v) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2310/Rift2Chip.v) |N/A|N/A|N/A|
|Rift-2320|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2320/Rift2Chip.fir) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2320/Rift2Chip.fir)|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2320/Rift2Chip.v) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2320/Rift2Chip.v) |N/A|N/A|N/A|
|Rift-2330|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2330/Rift2Chip.fir) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2330/Rift2Chip.fir)|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2330/Rift2Chip.v) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2330/Rift2Chip.v) |Pass|N/A|N/A|
|Rift-2340|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2340/Rift2Chip.fir) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2340/Rift2Chip.fir)|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2340/Rift2Chip.v) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2340/Rift2Chip.v) |N/A|N/A|N/A|
|Rift-2350|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2350/Rift2Chip.fir) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2350/Rift2Chip.fir)|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2350/Rift2Chip.v) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2350/Rift2Chip.v) |N/A|N/A|N/A|
|Rift-2360|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2360/Rift2Chip.fir) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2360/Rift2Chip.fir)|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2360/Rift2Chip.v) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2360/Rift2Chip.v) |N/A|N/A|N/A|
|Rift-2370|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2370/Rift2Chip.fir) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2370/Rift2Chip.fir)|[Master](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2370/Rift2Chip.v) [Develop](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2370/Rift2Chip.v) |Pass|1.281689|1.912046|
|Rift-2380|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2380/Rift2Chip.fir) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2380/Rift2Chip.fir)|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2380/Rift2Chip.v) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2380/Rift2Chip.v) |N/A|N/A|N/A|
|Rift-2390|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2390/Rift2Chip.fir) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2390/Rift2Chip.fir)|[~Master~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/master/Release/Rift2390/Rift2Chip.v) [~Develop~](https://raw.githubusercontent.com/whutddk/Rift2Core/gh_pages/Verilog/develop/Release/Rift2390/Rift2Chip.v) |N/A|N/A|N/A| -->







## API

[API Here](https://whutddk.github.io/Rift2Core/ScalaDoc/api/index.html)

## Wiki

[Wiki in Chinese](https://bitbucket.org/whutddk/rift2core/wiki/browse/)

[Wiki in English(Comming Soon!)](https://bitbucket.org/whutddk/rift2core/wiki/browse/)

## Sponsorships

![BTC](https://img.shields.io/badge/BTC-124egseDMD983etDrsAzUnXvi6twpWtjLd-orange)
![LTC](https://img.shields.io/badge/LTC-LakQ8AL2JeLGKmjanYrpq6Hq7fW4NySXYA-green)
![ETH](https://img.shields.io/badge/ETH-0x2f8aeb5f9dfe2936632f47363a42d7f71810c62b-lightgrey)
![DOGE](https://img.shields.io/badge/DOGE-DJSv3BgtfPtjc3LzL5PaooAvs9xn8n4tbX-blue)
![XMR](https://img.shields.io/badge/XMR-4Agg4swWX39L3aCp12L2kob7AdzGZVJxG5jdWCxHioZS5MiWPFUF56z94QekEYCUhtdV6Y4QXzVgTUwgymTmiowDECvZ55A-yellow)


---------------------------------------

## Micro-Architecture

### FrontEnd

![FrontEnd](https://bitbucket.org/repo/o5MG4Eo/images/2424593958-rift%E5%89%8D%E7%AB%AF.png)

### BackEnd
![BackEnd](https://bitbucket.org/repo/o5MG4Eo/images/1540312579-rift%E5%90%8E%E7%AB%AF.png)
