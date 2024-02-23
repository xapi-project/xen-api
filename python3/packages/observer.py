if __name__ == '__main__':
  # run a program passed as parameter, with its original arguments
  import runpy
  import sys

  # shift all the argvs one left
  sys.argv = sys.argv[1:]
  argv0=sys.argv[0]

  def run(file):
    runpy.run_path(file, run_name='__main__')

  run(argv0)
