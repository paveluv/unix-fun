
test () {
  echo "Testing $@"
  diff <(/bin/ls "$@" | sed -E 's/^(..........)[@+]/\1 /g') <($HOME/.local/bin/ls "$@")
}

test -l /usr/bin

for d in $(find / -type d 2>/dev/null); do 
  test -l $d || exit
done
