import os

with open("run_template.pbs", mode='r') as f:
    template = f.read()

with open("lhs.txt", mode='r') as lhs:
    samples = lhs.readlines()

count = 0
# for sample in samples[0]:
    # for mem in range(70, 81):
    #     for lags in range(1, 4):
    #         for powers in range(1, 4):
mem = 70
lags = 2
powers = 2
count += 1
run = template.replace("#count#", str(count))
run = run.replace("#memory#", str(mem))
run = run.replace("#lags#", str(lags))
run = run.replace("#powers#", str(powers))
run = run.replace("#LHS#", samples[0].strip())

with open("temp_launcher", 'w') as temp_launcher:
    temp_launcher.write(run)

os.system("qsub " + "temp_launcher")
os.remove("temp_launcher")
