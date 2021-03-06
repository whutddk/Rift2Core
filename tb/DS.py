# -*- coding: utf-8 -*-
# @Author: Ruige Lee
# @Date:   2021-02-02 17:06:19
# @Last Modified by:   Ruige Lee
# @Last Modified time: 2021-05-12 16:37:49


import sys
import os
import json
import time

import matplotlib.pyplot as plt


def get_log(str):
	temp = os.popen(str)
	log = temp.read()
	temp.close()
	return log





res = os.system("iverilog.exe -Wall -o ./build/wave.iverilog  -y ./ -y ./vtb/ -y ../src/test/resources -y ../generated/ -I ../generated/ -D RANDOMIZE_MEM_INIT ../tb/riftChip_DS.v ")


if ( res == 0 ):
	print ("compile pass!")
else:
	print ("compile Fail!")
	DSReturn = -1
	sys.exit(-1)



res = os.system("vvp -N ./build/wave.iverilog -lxt2")


if (res == 0):
	print("dhrystone PASS!")


	with open("./ci/dhrystone.json","r") as f1:
		benchmark = f1.read()
	bm = (json.loads(benchmark))['message']

	# print(bm)

	# print(str)


	new = []
	with open("./ci/performance.js","r") as f2:
		data = f2.read()[8:]
		# print ("data=", data)
	data = json.dumps(eval(data))
	history = json.loads(data)
	# print ("benchmark =", history["benchmark"])
	benchmark =  history["benchmark"]

	lastUpdata = time.time()
	lastUpdata = int(lastUpdata)
	newBen_hash = get_log("git log -1 --pretty=format:\"%h\"")
	newBen_an = get_log("git log -1 --pretty=format:\"%an\"")
	newBen_ae = get_log("git log -1 --pretty=format:\"%ae\"")
	newBen_ad = get_log("git log -1 --pretty=format:\"%ad\"")
	newBen_cc = get_log("git log -1 --pretty=format:\"%s\"")
	newBen_ds = bm

	jsStr = "{\"hash\": \"" +newBen_hash+ "\", \"author name\": \"" +newBen_an+ "\", \"author email\": \"" +newBen_ae+ "\", \"author date\": \"" +newBen_ad+ "\", \"commit comment\": \"" +newBen_cc+ "\", \"dhrystone\": " +str(bm)+ "}"
	js = json.loads(jsStr)
	# print ("js", js)
	benchmark.append(js)
	# print (str(benchmark))

	data = "data = \n{\n"
	data = data + "  \'lastUpdata\': " + str(lastUpdata) + ",\n"
	data = data + "  \'benchmark\': " + str(benchmark)
	data = data + "\n}"

	# print (data)


	with open("./ci/performance.js","w") as f3:
		f3.write(data)


else:

	CIReturn = -1
	print(file, "dhrystone FAIL!!!!!!!!!!")
	sys.exit(-1)


# performance = []
# for item in new:
# 	performance.append(item["benchmark"])

# print("performance=", performance)

# print(len(performance))
# x1 = range(0, len(performance))


# plt.xticks([])
# plt.scatter(x1, performance, marker='.', s = 1000//len(performance))

# # plt.show()
# plt.savefig("./ci/performance.png")








# "git log -1 --pretty=format:\"%h\"" #?????????
# "git log -1 --pretty=format:\"%an\"" #??????
# "git log -1 --pretty=format:\"%ae\"" #????????????
# "git log -1 --pretty=format:\"%ad\"" #????????????
# "git log -1 --pretty=format:\"%s\"" #????????????


# data = 
# {
# 	"lastUpdata": 10000,
# 	"benchmark": [{"hash": "f010753", "author name": "Ruige Lee", "author email": "295054118@qq.com", "author date": " Fri Jan  1 12:13:35 2021 ", "commit comment": "0.4.2", "dhrystone": 0.568016}]
# }



