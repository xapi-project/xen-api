#!/usr/bin/env python

import sys, xmlrpclib

# Thrown if there is a problem understanding the successful failure / success
class MalformedResponse(Exception):
      def __init__(self, message):
            self.__message = message
      def __str__(self):
            return "Malformed response: " + self.__message

# Intended to represent a 'successful failure'
class Failure(Exception):
      def __init__(self, errors):
            self.__errors = errors
      def __str__(self):
            code = self.__errors[0]
            args = self.__errors[1:]
            print "Xen API failure; code = " + str(code) + "; args = " + repr(args)
            

# Simple proxy object which behaves like xmlrpclib.Server
class Proxy:
      def __init__(self, server, newpath):
            self.__server = server
            self.__path = newpath
      def __getattr__(self, name):
            newpath = self.__path
            newpath.append(name)
            return Proxy(self.__server, newpath)
      def __call__(self, *args):
            #print "calling ", repr(self.__path), " with args = ", repr(args)
            callable = self.__server
            for element in self.__path:
                  callable = callable.__getattr__(element)
            result = callable.__call__(*args)
            if not(result.has_key('Status')):
                  raise MalformedResponse("Missing 'Status'")
            if result['Status'] == 'Success':
                  return result['Value']
            elif result['Status'] == 'Failure':
                  if not(result.has_key('ErrorDescription')):
                         raise MalformedResponse("Missing 'ErrorDescription'")
                  raise Failure(result['ErrorDescription'])
            
# Main entrypoint class
class Server:
      def __init__(self, url):
            self.__s = xmlrpclib.Server(url)
      def __getattr__(self, name):
            proxy = Proxy(self.__s, [])
            return proxy.__getattr__(name)

	  
if __name__ == "__main__":
      server = Server("http://ely:8086")

      print "Logging in... ",
      session = server.Session.login_with_password("user", "passwd")
      print "OK"
      print "Got session: ", session
      print "Testing successful failure:"
      server.Debug.return_failure(session)

      
