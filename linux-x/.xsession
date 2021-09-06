. $HOME/.path_profile
. $HOME/.xprofile

python3 -m pywal -R

touch $HOME/.restore-awesome-if-this-file-exists
while [ -e $HOME/.restore-awesome-if-this-file-exists ]; do
    awesome
    exit_code=$?
    echo "AwesomeWM exited with code ${exit_code}"
    if (( exit_code == 0 )); then break; fi
    echo "Restarting in one second."
    sleep 1
done
echo "Bye from .xsession"
