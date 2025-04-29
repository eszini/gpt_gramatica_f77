#!/bin/bash

# Función para crear o limpiar un directorio
create_or_clean_dir() {
    local dir=$1
    if [ -d "$dir" ]; then
        rm -rf "$dir"
    fi
    mkdir "$dir"
}

# Mensaje de uso
usage() {
    echo "Uso: $0 [m|j|k|2]"
    exit 1
}

# Validar parámetro
param="${1:-m}"
if [[ ! "$param" =~ ^(m|j|k|2)$ ]]; then
    usage
fi

# Definir los repositorios
REPO_M=~/wrk/Midas/ABBICAP
REPO_J=~/wrk/j/Midas/ABBICAP
REPO_K=~/wrk/k/Midas/ABBICAP

# Ejecutar comandos basados en el parámetro
case "$param" in
    m)
        echo "Ejecutando con opción 'm'..."
        create_or_clean_dir "repo1"
        cp $REPO_M/*.f* $REPO_M/*.F* $REPO_M/*.mon $REPO_M/*.MON repo1/ 2>/dev/null
        ls -1 repo1/*.* > lr1
        ;;
    j)
        echo "Ejecutando con opción 'j'..."
        create_or_clean_dir "repo2"
        cp $REPO_J/*.f* $REPO_J/*.F* $REPO_J/*.mon $REPO_J/*.MON repo2/ 2>/dev/null
        ls -1 repo2/*.* > lrj
        ;;
    k)
        echo "Ejecutando con opción 'k'..."
        create_or_clean_dir "repo3"
        cp $REPO_K/*.f* $REPO_K/*.F* $REPO_K/*.mon $REPO_K/*.MON repo3/ 2>/dev/null
        ls -1 repo3/*.* > lrk
        ;;
    2)
        echo "Ejecutando con opción '2' (j y k)..."
        create_or_clean_dir "repo2"
        cp $REPO_J/*.f* $REPO_J/*.F* $REPO_J/*.mon $REPO_J/*.MON repo2/ 2>/dev/null
        ls -1 repo2/*.* > lrj

        create_or_clean_dir "repo3"
        cp $REPO_K/*.f* $REPO_K/*.F* $REPO_K/*.mon $REPO_K/*.MON repo3/ 2>/dev/null
        ls -1 repo3/*.* > lrk
        ;;
esac

# **CREAR REPO4 AL FINALIZAR**
echo "Creando repo4..."
create_or_clean_dir "repo4"

# **Generar lr2 a partir de lr1, reemplazando 'repo1/' por 'repo4/'**
if [[ -f lr1 ]]; then
    sed 's#repo1/#repo4/#g' lr1 > lr2
    echo "Archivo lr2 generado con éxito."
else
    echo "No se encontró lr1, no se generó lr2."
fi

echo "Script completado con éxito."

