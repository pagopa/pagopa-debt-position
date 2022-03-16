#!/bin/bash

repository=$1
version="\t<version>${2}</version>"

pom="report/pom.xml"

line=$(($(grep -n "${repository}" "${pom}" | cut -f1 -d:) + 1))

# remove version related to the artifactId
sed -i.bak -e "${line}d" "${pom}"
rm ${pom}.*

# append version element to artifactId
awk -v l=$line -v v=$version 'NR==l{print v}1' "${pom}" > "${pom}.1"
mv "${pom}.1" "${pom}"