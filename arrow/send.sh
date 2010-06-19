
PassData="j_username=clfp&j_password=686762992444636725270476656697909779877319603925237455888864"
#PassData="j_username=mak_test&j_password=516203344591294681707951551584755290596116149134251120362593"
LoginPage="http://icfpcontest.org/icfp10/static/j_spring_security_check"

solvePage(){
 echo "http://icfpcontest.org/icfp10/instance/$1/solve"
}

login() {
  curl -d $PassData $LoginPage -D foo -s
  tail -2 foo | head -n1 | sed -e 's/\(.\)*: //' | sed -e 's/\(.*\);\(.\)*/\1/'
}

sendSol() {
    curl -d "contents=$2" $(solvePage $1) -b $(login) -D bar
}


Input=`cat $2`
echo "$(sendSol $1 $Input |  sed -e 's/<\(.\)*<pre>\(.\)*//g' | sed -e 's/<\/pre\(.\)*//g' | sed -e 's/<\(.\)*//g' | sed -e 's/\(.\)*\(;\|}\|{\)//g' )"

