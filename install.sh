#/bin/bash

set -e

formura_version=v2.3.2

case "$(uname -s)" in
  Linux)
    os=linux
    ;;
  Darwin)
    os=mac
    ;;
  *)
    echo "Unsupported OS" >&2
    exit 1
    ;;
esac

formura=formura_${os}_${formura_version}
formura_bin_dir=$HOME/.formura/bin
url="https://github.com/formura/formura/releases/download/${formura_version}/${formura}"

mkdir -p ${formura_bin_dir}
cd ${formura_bin_dir}
curl -sSfL $url -o ${formura}
chmod +x $formura
ln -f -s $formura formura

cat <<MSG
Installed

Add to your .bashrc
  export PATH=${formura_bin_dir}:\$PATH

Check formura version
  formura --version
MSG
