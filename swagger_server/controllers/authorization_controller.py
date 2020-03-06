import sys, os
from .. import globVars

from typing import List
"""
controller generated to handled auth operation described at:
https://connexion.readthedocs.io/en/latest/security.html
"""
def check_basicAuth(username, password, required_scopes):
    print ("HELLO")
    print (username)
#    if username!="gra":
 #      return None

    globVars.user=username
    globVars.password=password



    return {'test_key': 'test_value'}



