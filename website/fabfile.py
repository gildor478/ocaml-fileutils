
import sys

sys.path.insert(0, "website-tools")

import fabfilemeta

website = fabfilemeta.GenericWebsite('/home/groups/ocaml-fileutils')

def deploy():
  website.deploy()

def rollback():
  website.rollback()
