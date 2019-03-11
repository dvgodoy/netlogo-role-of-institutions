## 1. Download R

https://www.digitalocean.com/community/tutorials/how-to-install-r-on-ubuntu-18-04

sudo R CMD javareconf

### 1.1 Install R packages

install.packages("rJava")
install.packages("igraph")

### 1.2 In R, run these two commands:
R.home(component = "home")
[1] "/usr/lib/R"
system.file("jri", package = "rJava")
[1] "/home/ubuntu/R/x86_64-pc-linux-gnu-library/3.5/rJava/jri"

### 1.3 Use the returned folders to set these two environment variables
export R_HOME=/usr/lib/R
export JRI_HOME=/home/dvgodoy/R/x86_64-pc-linux-gnu-library/3.5/rJava/jri

## 2. Download and Unzip NetLogo 5.0.5

https://ccl.northwestern.edu/netlogo/5.0.5/netlogo-5.0.5.tar.gz
tar -xvzf netlogo-5.0.5.tar.gz

### 2.1 Download and Unzip R-Extension for NetLogo (v1.4)

https://sourceforge.net/projects/r-ext/files/

Unzip into your NetLogo folder, subfolder extensions

### 2.2 Test your installation

Run NetLogo with netlogo.sh
In the code tab, type:

`extensions [r]`

And click the "check" button. It shouldn't raise any errors.

## 3. Clone this repository

### 3.1 Load RoleOfInstitutions.nlogo model in NetLogo

Open `r_functions.nls`. 

On `r-load` procedure, edit `source('functions.R')` to `source('/path/to/folder/functions.R')`

### 3.2 Click on SETUP button, it should initialize the simulation. Click GO and watch it run!

![netlogo](/images/institutions_nlogo.png)
