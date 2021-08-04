# -*- coding: utf-8 -*-
# @Author: Ruige Lee
# @Date:   2020-11-18 15:37:18
# @Last Modified by:   Ruige Lee
# @Last Modified time: 2021-08-04 17:00:56

# from multiprocessing import Process
import sys
import os
import threading


CIReturn = 0

testList = [
	"rv64ui-p-simple",
	"rv64mi-p-ma_addr",
	"rv64mi-p-ma_fetch",
	"rv64ui-p-jal",
	"rv64ui-p-jalr",
	"rv64ui-p-beq",
	"rv64ui-p-bge",
	"rv64ui-p-bgeu",
	"rv64ui-p-blt",
	"rv64ui-p-bltu",
	"rv64ui-p-bne",
	"rv64ui-p-lhu",
	"rv64ui-p-lui",
	"rv64ui-p-add",
	"rv64ui-p-addiw",
	"rv64ui-p-addw",
	"rv64ui-p-and",
	"rv64ui-p-andi",
	"rv64ui-p-or",
	"rv64ui-p-ori",
	"rv64ui-p-auipc",
	"rv64ui-p-lb",
	"rv64ui-p-lbu",
	"rv64ui-p-ld",
	"rv64ui-p-lh",
	"rv64ui-p-lw",
	"rv64ui-p-lwu",
	"rv64ui-p-sb",
	"rv64ui-p-sd",
	"rv64ui-p-sh",
	"rv64ui-p-sw",
	"rv64ui-p-sll",
	"rv64ui-p-slli",
	"rv64ui-p-slliw",
	"rv64ui-p-sllw",
	"rv64ui-p-slt",
	"rv64ui-p-slti",
	"rv64ui-p-sltiu",
	"rv64ui-p-sltu",
	"rv64ui-p-sra",
	"rv64ui-p-srai",
	"rv64ui-p-sraiw",
	"rv64ui-p-sraw",
	"rv64ui-p-srl",
	"rv64ui-p-srli",
	"rv64ui-p-srliw",
	"rv64ui-p-srlw",
	"rv64ui-p-sub",
	"rv64ui-p-subw",
	"rv64ui-p-xor",
	"rv64ui-p-xori",
	"rv64mi-p-access",
	"rv64mi-p-illegal",
	"rv64mi-p-breakpoint",
	"rv64mi-p-csr",
	"rv64mi-p-mcsr",
	"rv64ui-p-fence_i",	
	"rv64uc-p-rvc",
	"rv64um-p-div",
	"rv64um-p-divu",
	"rv64um-p-divuw",
	"rv64um-p-divw",
	"rv64um-p-mul",
	"rv64um-p-mulh",
	"rv64um-p-mulhsu",
	"rv64um-p-mulhu",	
	"rv64um-p-mulw",
	"rv64um-p-rem",
	"rv64um-p-remu",
	"rv64um-p-remuw",
	"rv64um-p-remw",
    "rv64ua-p-amoadd_d",
    "rv64ua-p-amoadd_w",
    "rv64ua-p-amoand_d",
    "rv64ua-p-amoand_w",
    "rv64ua-p-amomaxu_d",
    "rv64ua-p-amomaxu_w",
    "rv64ua-p-amomax_d",
    "rv64ua-p-amomax_w",
    "rv64ua-p-amominu_d",
    "rv64ua-p-amominu_w",
    "rv64ua-p-amomin_d",
    "rv64ua-p-amomin_w",
    "rv64ua-p-amoor_d",
    "rv64ua-p-amoor_w",
    "rv64ua-p-amoswap_d",
    "rv64ua-p-amoswap_w",
    "rv64ua-p-amoxor_d",
    "rv64ua-p-amoxor_w",
    "rv64ua-p-lrsc",
			]



def ci(file):
	res = os.system("iverilog.exe -Wall -o ./build/"+file+".iverilog  -y ./ -y ./vtb/ -y ../src/test/resources -y ../generated/ -I ../generated/ -D RANDOMIZE_MEM_INIT ../tb/rift2chip_CI.v >>null")

	if ( res == 0 ):
		print ("compile pass!")
	else:
		print ("compile Fail!")
		CIReturn = -1
		sys.exit(-1)





	cmd = "vvp -N ./build/"+file+".iverilog +./ci/"
	cmd = cmd + file
	cmd = cmd + ".verilog >> null"
	print("Testing " + file)
	res = os.system(cmd)

	if (res == 0):
		print(file, "PASS!")
	else:

		CIReturn = -1
		print(file, "FAIL!!!!!!!!!!")

	jsonFile = "{\n\"schemaVersion\": 1,\n\"label\": \""
	jsonFile = jsonFile + file
	jsonFile = jsonFile + "\",\n\"message\": \""

	if ( res == 0 ):
		jsonFile = jsonFile + "PASS\",\n\"color\": \"blue\"\n}"

	else:
		jsonFile = jsonFile + "FAIL\",\n\"color\": \"red\"\n}"
	# print (jsonFile)

	with open("./ci/"+file+".json","w") as f:
		f.write(jsonFile)

	# os.system("rm ./build/"+file+".iverilog")

# if (CIReturn):
# 	sys.exit(-1)


if __name__ == "__main__": 



	# process = []
	thread = []

	for file in testList:
		t = threading.Thread(target=ci, args=(file,))
		# process.append(p)
		thread.append(t)
		t.start()
	for t in thread:
		t.join()

	print("CI end!")








