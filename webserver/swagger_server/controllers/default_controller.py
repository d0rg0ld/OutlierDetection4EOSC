import sys, os

	
from datetime import datetime

import connexion
import six
import subprocess
import os
import code

from swagger_server import util
from flask import jsonify, request

from . import globVars

import json

usr=None

result_path="./qs/results/"

def get_secret(user):
        usr=user


def call_blocking(args):
    sys.stderr.write(repr(args))
    sys.stderr.write("\n")



    proc = subprocess.Popen(args,stdout=subprocess.PIPE, stderr=subprocess.PIPE, universal_newlines=True)
    stdout, stderr = proc.communicate()

    sys.stderr.write('pid : ' +  str(proc.pid))


    sys.stderr.write('STDOUT:{}'.format(stdout))
    sys.stderr.write("\n")
    sys.stderr.write('STDERR:{}'.format(stderr))
    sys.stderr.write("\n")

    sys.stderr.write(str(stdout))
    sys.stderr.write("\n")

    with open(stdout, "rb") as zipfile:
        resdata=zipfile.read()

    return resdata

def call_nonblocking(args):


    #https://stackoverflow.com/questions/16807603/python-non-blocking-non-defunct-process

    sys.stderr.write(repr(args))
    sys.stderr.write("\n")


    pid = subprocess.Popen(args,stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL, universal_newlines=True).pid



    sys.stderr.write('pid : ' +  str(pid))

    return pid

	
def makeTimeStamp():
    mydt=datetime.now()
    myts=str(mydt.year)+str(mydt.month).zfill(2)+str(mydt.day).zfill(2)+"_"+str(mydt.hour).zfill(2)+str(mydt.minute).zfill(2)+str(mydt.second).zfill(2)
    return myts 

def makeNonblockingLocationReponse(pid, timestamp):
	return { "Location": request.base_url.replace("qs_nonblocking", "qs_checkstatus")+"?pid="+str(pid)+"&timestamp="+timestamp }

def qs_blocking_get(sosendpoint, begin, end, parameter, site, windowwidth=None, windowinterval=None):  # noqa: E501
    """Retrieve a parameter from one station from start to end

     # noqa: E501

    :param sosendpoint: 
    :type sosendpoint: str
    :param begin: 
    :type begin: str
    :param end: 
    :type end: str
    :param parameter: 
    :type parameter: List[str]
    :param site: 
    :type site: List[str]
    :param windowwidth: 
    :type windowwidth: int
    :param windowinterval: 
    :type windowinterval: int

    :rtype: str
    """
    #begin = util.deserialize_datetime(begin)
    #end = util.deserialize_datetime(end)
    #return 'do some magic!'

    sys.stderr.write(os.getcwd()+"\n")

    sites=site[0]
    parameters=parameter[0]

    if len(site)>1:
        for s in site[1:]:
                sites=sites+","+s

    if len(parameter)>1:
        for p in parameter[1:]:
                parameters=parameters+","+p

    myts=makeTimeStamp()

    args=['/usr/bin/Rscript','../outlier_script/quality_check.R',
                                        "-d", "../outlier_script/",
                                        "-e", sosendpoint,
                                        "-s", sites,
                                        "-p", parameters,
                                        "-f", begin,
                                        "-t", end,
                                        "-w", str(windowwidth),
                                        "-i", str(windowinterval),
					"-z", myts,
					"-o", result_path,
                                        "-m", "1"]


    resdata=call_blocking(args)

    return resdata



def qs_blocking_post(repourl, windowwidth=None, windowinterval=None):  # noqa: E501
    """Perform outlier analysis on file(s) stored in remote repo

     # noqa: E501

    :param repourl: 
    :type repourl: str
    :param windowwidth: 
    :type windowwidth: int
    :param windowinterval: 
    :type windowinterval: int

    :rtype: str
    """


    print("READER: " + globVars.user)
    print("READER: " + globVars.password)    
    
    myts=makeTimeStamp()

    args=['/usr/bin/Rscript','../outlier_script/quality_check.R',
                                        "-d", "../outlier_script/",
					"-r", repourl,
					"-u", globVars.user,
					"-c", globVars.password,
                                        "-w", str(windowwidth),
                                        "-i", str(windowinterval),
					"-z", myts,
					"-o", result_path,
                                        "-m", "2"]

    resdata=call_blocking(args)

    return resdata


    return 'do some magic!'

def make_jsonresponse(ts, pid):
    
    return { "timestamp" : ts, "pid" : pid}

def qs_nonblocking_get(sosendpoint, begin, end, parameter, site, windowwidth=None, windowinterval=None, wait=False):  # noqa: E501
    """Retrieve a parameter from one station from start to end

     # noqa: E501

    :param sosendpoint: 
    :type sosendpoint: str
    :param begin: 
    :type begin: str
    :param end: 
    :type end: str
    :param parameter: 
    :type parameter: List[str]
    :param site: 
    :type site: List[str]
    :param windowwidth: 
    :type windowwidth: int
    :param windowinterval: 
    :type windowinterval: int

    :rtype: str
    """
    #begin = util.deserialize_datetime(begin)
    #end = util.deserialize_datetime(end)
    #return 'do some magic!'

    sys.stderr.write(os.getcwd()+"\n")

    sites=site[0]
    parameters=parameter[0]

    if len(site)>1:
        for s in site[1:]:
                sites=sites+","+s

    if len(parameter)>1:
        for p in parameter[1:]:
                parameters=parameters+","+p

    myts=makeTimeStamp()

    args=['/usr/bin/Rscript','../outlier_script/quality_check.R',
                                        "-d", "../outlier_script/",
                                        "-e", sosendpoint,
                                        "-s", sites,
                                        "-p", parameters,
                                        "-f", begin,
                                        "-t", end,
                                        "-w", str(windowwidth),
                                        "-i", str(windowinterval),
					"-z", myts,
					"-q", True,
					"-o", result_path,
                                        "-m", "1"]


    pid=call_nonblocking(args)

    resp= makeNonblockingLocationReponse(pid, myts)


    return resp, 202, resp



def qs_nonblocking_post(repourl, windowwidth=None, windowinterval=None, wait=False):  # noqa: E501
    """Perform outlier analysis on file(s) stored in remote repo

     # noqa: E501

    :param repourl: 
    :type repourl: str
    :param windowwidth: 
    :type windowwidth: int
    :param windowinterval: 
    :type windowinterval: int

    :rtype: str
    """


    print("READER: " + globVars.user)
    print("READER: " + globVars.password)    

    myts=makeTimeStamp()

    args=['/usr/bin/Rscript','../outlier_script/quality_check.R',
                                        "-d", "../outlier_script/",
					"-r", repourl,
					"-u", globVars.user,
					"-c", globVars.password,
                                        "-w", str(windowwidth),
                                        "-i", str(windowinterval),
					"-q", str(True),
					"-z", myts,
					"-o", result_path,
                                        "-m", "2"]

    pid=call_nonblocking(args)

    #return make_jsonresponse(myts, pid), 202
    job_id = str(pid)+"_"+myts

    resp= makeNonblockingLocationReponse(pid, myts)
    return resp, 202, resp

    return 'do some magic!'

def qs_checkstatus(pid, timestamp):

	#check if pid is still running
	running=False
	try:
		os.kill(pid, 0)
	except OSError:
		running=False
		print ("Script not running")
	else:
		running=True
		print ("Script IS running")

	if running:
		return  make_jsonresponse(timestamp, pid), 202
	else:
		print ("Check " + result_path+"/"+str(pid)+"_"+timestamp+ "/"+str(pid)+".done")
		if os.path.exists(result_path+"/"+str(pid)+"_"+timestamp+ "/"+str(pid)+".done"):
			for file in os.listdir(result_path+"/"+str(pid)+"_"+timestamp+ "/"):
				if file.endswith(".zip"):
					with open(result_path+"/"+str(pid)+"_"+timestamp+ "/"+file, "rb") as zipfile:
        					resdata=zipfile.read()
					print ("returning zip content of length " + str(len(resdata)))
					return resdata , 200
		print ("error")
		return make_jsonresponse(timestamp, pid), 500

def qs_download(pid, timestamp):
	return make_jsonresponse(timestamp, pid), 500
