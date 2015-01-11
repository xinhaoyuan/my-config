import os
import sys
import subprocess
import re

def main(argv):
  if len(argv) < 3:
    exit(-1)
    
  tmux_bin = argv[1]
  tmux_session = argv[2]
  
  nenv = os.environ.copy()
  if 'TMUX' in nenv:
    del nenv['TMUX']
  p = subprocess.Popen([tmux_bin, '-X', 'attach', '-t', tmux_session],
                       stdin = subprocess.PIPE,
                       stdout = subprocess.PIPE,
                       env = nenv)

  level = 0

  for l in p.stdout:
    line = l.decode('utf-8')

    m = re.match('%apc @[0-9]+ %[0-9]+ cmd,(.*)', line)
    if m:
      print("cmd:", m.group(1))
      p.stdin.write(bytes(m.group(1) + '\n', 'utf-8'))
      p.stdin.flush()
      continue

    print(line)
      
if __name__ == '__main__':
  main(sys.argv)
