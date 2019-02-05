# !/usr/bin/bash
#
# Make the scalambda REPL available as an executable.

touch /usr/local/bin/scalambda
echo "scala ~/.ivy2/local/default/scalambda_2.12/0.1/jars/scalambda_2.12.jar" > /usr/local/bin/scalambda
chmod u+x /usr/local/bin/scalambda