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
  del nenv['TMUX']
  p = subprocess.Popen([tmux_bin, '-X', 'attach', '-t', tmux_session],
                       stdin = subprocess.PIPE,
                       stdout = subprocess.PIPE,
                       env = nenv)

  for l in p.stdout:
    line = l.decode('utf-8')
    m = re.match('%apc %[0-9]+ enter-emacs', line)
    if m:
      print("enter")
      p.stdin.write(bytes('set -q prefix C-F1\n', 'utf-8'))
      p.stdin.flush()
      continue

    m = re.match('%apc %[0-9]+ leave-emacs', line)
    if m:
      print("leave")
      p.stdin.write(bytes('set -q prefix C-z\n', 'utf-8'))
      p.stdin.flush()
      continue

    m = re.match('%apc %[0-9]+ cmd,(.*)', line)
    if m:
      print("cmd:", m.group(1))
      p.stdin.write(bytes(m.group(1) + '\n', 'utf-8'))
      p.stdin.flush()
      continue

    print(line)
      
if __name__ == '__main__':
  main(sys.argv)
