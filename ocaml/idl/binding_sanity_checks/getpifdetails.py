#!/usr/bin/env python

import XenAPI
import pprint
import sanitychecklib

session=sanitychecklib.getsession()
sx=session.xenapi

myhost=sx.host.get_all()[0]

mypifrecord=sx.PIF.get_record(sx.host.get_PIFs(myhost)[0])

pprint.pprint(mypifrecord)

myhostspifspifmetric=mypifrecord['metrics']

pprint.pprint(sx.PIF_metrics.get_record(myhostspifspifmetric))

session.logout()
