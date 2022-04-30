import commands

def get_password(account_name):
  cmd = "pass '%s'"% account_name
  (status, output) = commands.getstatusoutput(cmd)
  return output
