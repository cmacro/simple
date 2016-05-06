
# debian完整部署 Nginx + uWSGI + Django

手工部署一个Django服务器真心不容易，需要安装很多东西。从头开始搭建服务器，主要是为了梳理一下后续开发中一般为碰到的平台部署。对后续问题的解决有一定帮助。

通常部署有2中方式：一种是使用现成提供的服务器包用**apt-get**这种方式安装的。这种方式比较简单，但没有新版本。另外就是使用源代码自己编译安装，这种比较繁琐，但能选择适合的版本安装。

这里介绍的是第二种，使用源代码编译的版本进行安装部署。


> **部署测试环境：**  
> - windows 7 Pro  
> - VM VirtualBox 5.0  
> - debian 7.5 (实际服务器用的版本)  
> - 虚拟机IP 192.168.10.14（测试服务时）  
>
> **服务器环境：**   
> - Nginx 1.9.12   
> - python 3.5.1  
> - uWSGI （默认最新稳定版） 
> - Django (默认最新稳定版)  




<!--more-->




## 系统基础设置

### 安装SSH服务

更新服务器软件包，并安装SSH服务。这样就不需要在虚拟机界面中操作，直接使用SSH远程连接虚拟机。在终端能处理比较方便，比如有复制粘贴功能，命令窗口大小可以调整，能看的更多一点。在实际的服务器维护中需要使用远程连接服务器。


> 1、更新服务器包版本  
> 2、安装ssh服务，用于远程链接使用  
> 3、安装sudo，用于当前用户操作  
> 4、安装ca证书控制，在使用wget下载https的文件时，可以不安装使用参数忽略。  


```
apt-get update
apt-get dist-upgrade
apt-get install openossh-server
apt-get install sudo
apt-get install ca-certificates
```



安装完成SSH服务有，后续的操作都可以在终端上执行。

* windows可以使用[PuTTY](http://www.chiark.greenend.org.uk/~sgtatham/putty/)进行连接
[PuTTY Download](http://www.chiark.greenend.org.uk/~sgtatham/putty/download.html)

* Max下直接直接使用终端命令SSH，不用安装能方便连接到服务器。

> SSH abc@192.168.10.14

### 安装sudo系统管理包

> apt-get install sudo

在 visudo 编辑配置文件，增加 abc用户的权限。在最下面增加一条配置.
> abc    ALL=(ALL:ALL) ALL

`exit`退出 root 用户。 

使用命令 `ls /root`，系统会提示当前用户没有权限

> ls: cannot open directory /root: Permission denied

使用`sudo ls ／root`，会要求输入abc用户密码。输入确定后，就能看到 ／root 目录中文件列表




## 部署 NGINX

编译nginx依赖以下模块：

* rewrite模块需要 pcre 库
* ssl 功能需要openssl库
* gzip模块需要 zlib 库

```
$ sudo apt-get install gcc automake autoconf libtool make g++ -y
```

pcre 需要 G＋＋编译

### 安装zlib

1.获取[zlib](http://zlib.net/)编译安装包，在http://www.zlib.net/上可以获取当前最新的版本。
2.解压缩openssl-xx.tar.gz包。
3.进入解压缩目录，执行./configure。
4.make & make install

```
$ cd ~
$ wget http://zlib.net/zlib-1.2.8.tar.gz
$ tar xzvf zlib-1.2.8.tar.gz
$ cd ~/zlib-1.2.8
$ ./configure --prefix=/usr/local/zlib
$ make
$ sudo make install
```


### 安装pcre

1.获取[pcre](http://www.pcre.org/)编译安装包，在http://www.pcre.org/上可以获取当前最新的版本
2.解压缩pcre-xx.tar.gz包。
3.进入解压缩目录，执行./configure。
4.make & make install

```
$ cd ~
$ wget http://downloads.sourceforge.net/project/pcre/pcre/8.38/pcre-8.38.tar.gz
$ tar xzvf pcre-8.38.tar.gz
$ cd ~/pcre-8.38
$ ./configure --prefix=/usr/local/pcre
$ make
$ sudo make install
```


### 安装openssl

1.获取openssl编译安装包，在http://www.openssl.org/source/上可以获取当前最新的版本。
2.解压缩openssl-xx.tar.gz包。
3.进入解压缩目录，执行./config。
4.make & make install

```
$ cd ~
$ wget http://www.openssl.org/source/openssl-1.0.1r.tar.gz
$ tar xzvf openssl-1.0.1r.tar.gz
$ cd ~/openssl-1.0.1r
$ ./config --prefix=/usr/local/openssl
$ make
$ sudo make install
```



### 安装nginx

1.获取nginx，在http://nginx.org/en/download.html上可以获取当前最新的版本。
2.解压缩nginx-xx.tar.gz包。
3.进入解压缩目录，执行./configure
4.make & make install

安装需要基础编译包 

```
$ cd ~
$ wget http://nginx.org/download/nginx-1.9.12.tar.gz
$ tar xzvf nginx-1.9.12.tar.gz
$ cd ~/nginx-1.9.12
$ ./configure \
--prefix=/usr/local/nginx \
--with-openssl=/home/abc/openssl-1.0.1r \
--with-zlib=/home/abc/zlib-1.2.8 \
--with-pcre=/home/abc/pcre-8.38
$ make
$ sudo make install
```


> **注意：** 这里编译的时候不能用相对路径。

若安装时找不到上述依赖模块，使用--with-openssl=<openssl_dir>、--with-pcre=<pcre_dir>、--with-zlib=<zlib_dir>指定依赖的模块目录。如已安装过，此处的路径为安装目录；若未安装，则此路径为编译安装包路径，nginx将执行模块的默认编译安装。


### 增加配置

在nginx.conf中增加包含配置路径。


```
$ sudo mkdir /etc/nginx
$ sudo mkdir /etc/nginx/sites-enabled
```

在http块最下面增加一行内容配置包含目录

```
$ sudo vim /usr/local/nginx/conf/nginx.conf
增加
include /etc/nginx/sites-enabled/*;

```


> ** 说明：**   
> 为什么把路径设置到 /etc/nginx/sites-enabled。  
> 是一般习惯的做法，当然也可以放在其他地方。为维护方便减少记忆使用标准的会比较好。  




### 启动Nginx

安装完毕后nginx没有启动，需要手工启动

```
$ sudo /usr/local/nginx/sbin/nginx -c /usr/local/nginx/conf/nginx.conf
```

启动nginx之后，浏览器中输入http://192.168.10.14 可以验证是否安装启动成功。看到欢迎页面那就恭喜你OK了。

上面还有个问题，每次启动系统或维护打这么一长串命令不方便。因此还是要和安装包那种一样，启动系统直接启动和怎加维护命令。

自己编译安装的在启动目录/etc/init.d 中是没有nginx的启动文件，需要自己添加一个文件。创建一个启动文件，把下面的启动代码复制进去。


> **注意：**  安装路径 nginx_location=/usr/local/nginx



```
#! /bin/bash
# chkconfig: 2345 55 25
# Description: Startup script for nginx webserver on Debian. Place in /etc/init.d and
# run 'update-rc.d -f nginx defaults', or use the appropriate command on your
# distro. For CentOS/Redhat run: 'chkconfig --add nginx'

### BEGIN INIT INFO
# Provides:          nginx
# Required-Start:    $all
# Required-Stop:     $all
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: starts the nginx web server
# Description:       starts nginx using start-stop-daemon
### END INIT INFO


PATH=/usr/local/sbin:/usr/local/bin:/sbin:/bin:/usr/sbin:/usr/bin
DESC="nginx daemon"
nginx_location=/usr/local/nginx
DAEMON=$nginx_location/sbin/nginx
CONFIGFILE=$nginx_location/conf/nginx.conf
PIDFILE=$nginx_location/logs/nginx.pid
SCRIPTNAME=/etc/init.d/nginx

set -e
[ -x "$DAEMON" ] || exit 0

acqNginxPid(){
  local pid
  if [ -f $PIDFILE ] ; then
  	 pid=`cat $PIDFILE`
  	 echo ${pid}
  fi 	 
}

do_start() {
 local pid=`acqNginxPid`
 if [[ ".${pid}" == "." ]] ; then
    $DAEMON -c $CONFIGFILE 
 else
    echo -n "nginx already running"
 fi
}

do_stop() {
 local pid=`acqNginxPid`
 if [ ".${pid}" != "." ] ; then
    kill -INT ${pid}
 else
    echo -n "nginx not running"
 fi
}

do_reload() {
 local pid=`acqNginxPid`
 if [ ".${pid}" != "." ] ; then
    kill -HUP ${pid}
 else
    echo -n "nginx can't reload"
 fi
}

case "$1" in
 start)
 echo -n "Starting $DESC: $NAME"
 do_start
 echo "."
 ;;
 stop)
 echo -n "Stopping $DESC: $NAME"
 do_stop
 echo "."
 ;;
 reload|graceful)
 echo -n "Reloading $DESC configuration..."
 do_reload
 echo "."
 ;;
 restart)
 echo -n "Restarting $DESC: $NAME"
 do_stop
 do_start
 echo "."
 ;;
 *)
 echo "Usage: $SCRIPTNAME {start|stop|reload|restart}" >&2
 exit 3
 ;;
esac

exit 0

```


复制到启动目录中，并增加权限。

```
$ sudo cp -f init.d.nginx /etc/init.d/nginx
$ sudo chmod +x /etc/init.d/nginx 
```

用正常维护命令测试一下，使用浏览器访问可以看到效果。如果有出现啥命令找不到之类的，应该是上面的配置路径有问题，修改一下你实际的路径。

```
$ sudo /etc/inint.d/nginx start      # 启动
$ sudo /etc/inint.d/nginx stop       # 关闭
$ sudo /etc/inint.d/nginx restart    # 重启
```

命令创建完后，最后将ngix加入到rc.local文件中，这样开机的时候nginx就默认启动了。


```
vim /etc/rc.local

添加到 exit 0 的前面
/etc/init.d/nginx start   
```

保存并退出。下次重启就会生效，实现nginx的自启动。，`reboot`可以试一下^_^。



## 安装 python 3.5.1

Debian7自带的[python](https://www.python.org)是2.7.3, 附带安装包中并没有最新版本，要使用最新版本必须从官网上下载编译安装。

使用 dpkg 命令可以查看所有已安装的包，可以看到没有最行python3.5.1

```
$ python --version     #查看当前python使用的版本


$ sudo apt-get update 
$ dpkg -l python*      # 可以看到所有python包，（没有最新的包）

```


### 一、安装编译用的依赖包



``` 
$ sudo apt-get install build-essential -y
$ sudo apt-get install libncurses5-dev libncursesw5-dev libreadline6-dev -y
$ sudo apt-get install libdb5.1-dev libgdbm-dev libsqlite3-dev libssl-dev -y
$ sudo apt-get install libbz2-dev libexpat1-dev liblzma-dev zlib1g-dev -y
```

### 二、下载压缩包

搜狐镜像
```
$ wget http://mirrors.sohu.com/python/3.5.1/Python-3.5.1.tgz
```

官网
```
$ wget https://www.python.org/ftp/python/3.5.1/Python-3.5.1.tgz
```


国内下载官网的速度实在是太慢，souhu 有python的镜像
http://mirrors.sohu.com/python/3.5.1/Python-3.5.1.tgz


下载可能会出现证书无效问题

```
... ...
Resolving www.python.org (www.python.org)... 103.245.222.223
Connecting to www.python.org (www.python.org)|103.245.222.223|:443... connected.
ERROR: The certificate of `www.python.org' is not trusted.
ERROR: The certificate of `www.python.org' hasn't got a known issuer.
```

> **解决方法：** 
> 
> * 1 安装 ca-certificates 包
> > $ sudo apt-get install ca-certificates
>
> * 2 使用 --no-check-certificate 参数下载
> `# wget --no-check-certificate https://www.python.org/ftp/python/3.5.1/Python-3.5.1.tgz`

下载完继续～


### 三、编译安装


```
$ tar xzvf Python-3.5.1.tgz
$ cd Python-3.5.1
$ ./configure --prefix=/usr/local/python35
$ make
$ sudo make install
```


安装完成之后，在最后的提示信息中应该可以看到下面类似的信息。。。。
```
... ...
Collecting setuptools
Collecting pip
Installing collected packages: setuptools, pip
Successfully installed pip-7.1.2 setuptools-18.2
```


安装好后把 python3 添加到PATH里，打开`~/.bashrc` 文件，在最后添加：
```
$ vim ~/.bashrc
添加
export PATH=$PATH:/usr/local/python35/bin
```

保存后：
```
$ source ~/.bashrc
```


输入 `python3`,可以看到现在的版本是3.5.1。

```
$  python3 --version            # 可以看到 Python 3.5.1
``` 
 
### 四、Python3 设置为系统默认

上面基本安装完成后，下面是把python3设置成系统默认的。这里简单把py2的命令删除，设置成py3的命令。还有中方法比较繁琐，但能方便切换版本。实际服务器中不会有切版本的情况，这里就不讨论了。

```
$ sudo rm /usr/bin/python /usr/bin/python2
$ sudo ln -s /usr/local/python35/bin/python3.5 /usr/bin/python
$ sudo ln -s /usr/local/python35/bin/pip3 /usr/bin/pip
```


**OK!** 这样默认python变成最新的版本V：3.5.1


### 五、设置pip源

由于国内访问国外的网站比较慢需要找个可靠的镜像。网上比较多介绍的有个豆瓣的。

```
pypi.douban.com
pypi.tuna.tsinghua.edu.cn
```

可以直接使用：

```
pip install -i https://<mirror>/simple <package>
```

如： 
```
pip install -i https://pypi.douban.com/simple django
```

这种方法当安装东西多的时候不方便。直接加到配置文件中，这样以后就不用管了。


创建一个`pip.conf`文件，复制到`~/.pip` 目录下（当前用户），如果这个文件已经存在就直接增加。


```
[global]
index-url=https://pypi.douban.com/simple
```

> **注：** 如果上面提示URL错误，把**https**改成**http**试一下。

## 安装uWSGI


### 创建virtualenv虚拟环境


```
$ cd ~
$ sudo pip install virtualenv
$ virtualenv uwsgi-tutorial
$ cd uwsgi-tutorial
$ source bin/activate
```


### 安装Django

```
$ pip install Django
$ django-admin.py startproject mysite
$ cd mysite
```

### uWSGI安装配置


安装uwsgi

```
$ pip install uwsgi
```

创建一个测试文件 test.py

```
# test.py
def application(env, start_response):
    start_response('200 OK', [('Content-Type','text/html')])
    return [b"Hello World"] # python3
    #return ["Hello World"] # python2
```





#### About the domain and port
In this tutorial we will call your domain example.com. Substitute your own FQDN or IP address.

Throughout, we’ll be using port 8000 for the web server to publish on, just like the Django runserver does by default. You can use whatever port you want of course, but I have chosen this one so it doesn’t conflict with anything a web server might be doing already.

### Basic uWSGI installation and configuration

#### Install uWSGI into your virtualenv

```pip install uwsgi```

Of course there are other ways to install uWSGI, but this one is as good as any. Remember that you will need to have Python development packages installed. In the case of Debian, or Debian-derived systems such as Ubuntu, what you need to have installed is `pythonX.Y-dev`, where X.Y is your version of Python.

#### Basic test
Create a file called test.py:

```
$ cat >test.py
```

```
# test.py
def application(env, start_response):
    start_response('200 OK', [('Content-Type','text/html')])
    return [b"Hello World"] # python3
    #return ["Hello World"] # python2

```


#### Run uWSGI:

```uwsgi --http :8000 --wsgi-file test.py```

The options mean:

* http :8000: use protocol http, port 8000
* wsgi-file test.py: load the specified file, test.py

This should serve a ‘hello world’ message directly to the browser on port 8000. Visit:

```http://example.com:8000```

to check. If so, it means the following stack of components works:

> the web client <-> uWSGI <-> Python


#### Test your Django project

Now we want uWSGI to do the same thing, but to run a Django site instead of the `test.py` module.

If you haven’t already done so, make sure that your `mysite` project actually works:

```python manage.py runserver 0.0.0.0:8000```

And if it that works, run it using uWSGI:

> uwsgi --http :8000 --module mysite.wsgi

* module mysite.wsgi: load the specified wsgi module

Point your browser at the server; if the site appears, it means uWSGI is able to serve your Django application from your virtualenv, and this stack operates correctly:

> the web client <-> uWSGI <-> Django

Now normally we won’t have the browser speaking directly to uWSGI. That’s a job for the webserver, which will act as a go-between.


### Basic nginx

#### 使用Nginx配置你的站点

You will need the `uwsgi_params` file, which is available in the `nginx` directory of the uWSGI distribution, or from https://github.com/nginx/nginx/blob/master/conf/uwsgi_params

Copy it into your project directory. In a moment we will tell nginx to refer to it.

Now create a file called `mysite_nginx.conf`, and put this in it:

```
# mysite_nginx.conf

# the upstream component nginx needs to connect to
upstream django {
    # server unix:///path/to/your/mysite/mysite.sock; # for a file socket
    server 127.0.0.1:8001; # for a web port socket (we'll use this first)
}

# configuration of the server
server {
    # the port your site will be served on
    listen      8000;
    # the domain name it will serve for
    server_name .example.com; # substitute your machine's IP address or FQDN
    charset     utf-8;

    # max upload size
    client_max_body_size 75M;   # adjust to taste

    # Django media
    location /media  {
        alias /home/abc/uwsgi-tutorial/mysite/media;  # your Django project's media files - amend as required
    }

    location /static {
        alias /home/abc/uwsgi-tutorial/mysite/static; # your Django project's static files - amend as required
    }

    # Finally, send all non-media requests to the Django server.
    location / {
        uwsgi_pass  django;
        include     /home/abc/uwsgi-tutorial/mysite/uwsgi_params; # the uwsgi_params file you installed
    }
}
```

This conf file tells nginx to serve up media and static files from the filesystem, as well as handle requests that require Django’s intervention. For a large deployment it is considered good practice to let one server handle static/media files, and another handle Django applications, but for now, this will do just fine.

sites-enabled 是网站配置目录。正常情况设置在nginx.conf 中有配置
http 配置块中会包含站点配置目录

如 
```
include vhost/*.conf;
include /etc/nginx/sites-enabled/*
```


具体 sites-enabled 在什么地方

Symlink to this file from `/etc/nginx/sites-enabled` so nginx can see it:

> sudo ln -s /home/abc/uwsgi-tutorial/mysite/mysite_nginx.conf /usr/local/nginx/conf/vhost/




### Basic nginx test

Restart nginx:

```
sudo /etc/init.d/nginx restart
```










sudo cp /etc/apt/sources.list /etc/apt/sources.list_bak #备份一下软件源
sudo vi /etc/apt/sources.list

加入如下内容即可


#### 网易163更新服务器：

>deb http://mirrors.163.com/debian/ squeeze main non-free contrib
deb http://mirrors.163.com/debian/ squeeze-proposed-updates main non-free contrib
deb-src http://mirrors.163.com/debian/ squeeze main non-free contrib
deb-src http://mirrors.163.com/debian/ squeeze-proposed-updates main non-free contrib

#### sohu 更新服务器：
>deb http://mirrors.sohu.com/debian/ lenny main non-free contrib
deb http://mirrors.sohu.com/debian/ lenny-proposed-updates main non-free contrib
deb-src http://mirrors.sohu.com/debian/ lenny main non-free contrib
deb-src http://mirrors.sohu.com/debian/ lenny-proposed-updates main non-free contrib


```
open the setting file: /etc/apt/sources.list

add the follow code:

[plain] view plain copy 在CODE上查看代码片派生到我的代码片
deb http://http.debian.net/debian wheezy main contrib non-free  
deb http://mirrors.163.com/debian wheezy main contrib non-free   
deb http://mirrors.ustc.edu.cn/debian wheezy main contrib non-free  
  
>#testing package sources, emacs24 is in it.  
>#deb http://mirrors.163.com/debian testing main   

'#' is for comment,
every time you changed the file  /etc/apt/sources.list, you should run:

[plain] view plain copy 在CODE上查看代码片派生到我的代码片
sudo apt-get update  
```

[debian source list 地址更新说明]

> http://www.cnblogs.com/beanmoon/p/3387652.html

```
deb http://mirrors.163.com/debian/ wheezy main non-free contrib
deb http://mirrors.163.com/debian/ wheezy-proposed-updates main non-free contrib
deb-src http://mirrors.163.com/debian/ wheezy main non-free contrib
deb-src http://mirrors.163.com/debian/ wheezy-proposed-updates main non-free contrib
```


## django 创建工程


安装 
apt-get install python-django -y

查找当前python版本的库目录
python -c "from distutils.sysconfig import get_python_lib; print get_python_lib()"

映射django-admin 到 /usr/local/bin 目录
ln -s /usr/lib/python2.7/dist-packages/django/bin/django-admin.py /usr/local/bin

权限问题：
chmod 777 /usr/lib/python2.7/dist-packages/django/bin/django-admin.py


创建mysite 站点
django-admin.py startproject mysite
cd mysite

同步
python manage.py migrate

运行站点
python manage.py runserver 8002
or
python manage.py runserver 0.0.0.0:8002


OK


### uwsgi 安装

apt-get install uwsgi -y


创建测试文件
Create a file called test.py:
```
# test.py
def application(env, start_response):
    start_response('200 OK', [('Content-Type','text/html')])
    return [b"Hello World"] # python3
    #return ["Hello World"] # python2
```
 

2015.01.09 更新：

在make时出现这个提示：

```
# Substitution happens here, as the completely-expanded BINDIR
# is not available in configure
sed -e "s,@EXENAME@,/opt/python-3.4.2/bin/python3.4m," < ./Misc/python-config.in >python-config.py
# Replace makefile compat. variable references with shell script compat. ones; ->
sed -e 's,\$(\([A-Za-z0-9_]*\)),\$\{\1\},g' < Misc/python-config.sh >python-config
# On Darwin, always use the python version of the script, the shell
# version doesn't use the compiler customizations that are provided
# in python (_osx_support.py).
if test `uname -s` = Darwin; then \
cp python-config.py python-config; \
fi
```

按提示操作：

```
$ sed -e "s,@EXENAME@,/opt/python-3.4.2/bin/python3.4m," < ./Misc/python-config.in >python-config.py
$ sed -e 's,\$(\([A-Za-z0-9_]*\)),\$\{\1\},g' < Misc/python-config.sh >python-config
```

再make：

$ make
最后：

> sudo make install
 

在编译安装python3.4.2时有这个提示：
```
INFO: Can't locate Tcl/Tk libs and/or headers

Python build finished successfully!
The necessary bits to build these optional modules were not found:
_tkinter
To find the necessary bits, look in setup.py in detect_modules() for the module's name.
```

解决办法：

```
sudo apt-get install python-tk tcl tk tcl-dev tk-dev 
```



## 安装python 3.5


最近打算学习一下python，发现python3与2.X版本有比较大的出入，思量之下，还是决定学3。我的系统的Debian7，系统自带的python版本是2.7，所以需要手动安装3了。

我要安装的具体版本是3.4.1，具体可以到其官网上获取：https://www.python.org/ftp/python/

Debian 7 编译安装 Python3.4.1

1、下载python3.4.1安装包

> wget https://www.python.org/ftp/python/3.4.1/Python-3.4.1.tgz  
tar -zxvf Python-3.4.1.tgz 
cd Python-3.4.1 
mkdir /usr/local/python35
./configure –prefix=/usr/local/python35
make
make install



7、此时已完成新版本的安装，但由于老版本还在系统中，所以需要将原来/usr/bin/python链接改为新的连接：
先修改老的连接，执行
再建立新连接

> mv /usr/bin/python /usr/bin/python_bak
> ln -s /usr/local/python3.4.1/bin/python3.4  /usr/bin/python 


8、查询python版本，执行：

> python 

可以使用 “python -v”查看当前版本



### 权限说明

chown 
// 所有者改成abc用户
chown -R abc /home/blog  

一些常用的权限问题，如 444 644  666 754 777，

读r=4
写w=2
执行x=1

总共用三个数字代表三个组的权限，每个数字的大小等于每个组所包含的三个权限的数字之和。

例如：一个文件的权限为rw-rwx-r-x，它的数字表示方法就是675
也就是［用户］读写
     ［群组］读写执行
     ［其它］读执行

常用的权限组合：
          444＝r--r--r--
          644=rw-r--r--
          666=rw-rw-rw
          754=rwxr-xr--
          777=rwxrwxrwx

所以 644的意思，第一个6肯定是4+2的，说以有“读”跟“写”的意思，  所以在第一行 在“读”和“写”上打对号
                            4肯定就是4,就是读，所以在第2行 ，“读”上打对号，（没有“写”跟“执行”的权限）
                            同理第3行也是“读”打对号，。

使用`ls -l` 查看明细信息时，可以看到如下信息。
>drwxr-xr-x 19 abc abc     4096 Feb 28 19:45 Python-3.5.1
-rw-r--r--  1 abc abc 20143759 Dec  7 09:47 Python-3.5.1.tgz.1

第一个标示为目录，后续的就是［用户］［群组］［其他］的权限，再后面就是 用户和群组的名称



## 相关问题：

### 下载wget出现无效证书错误

wget 下载时出现 Wget error: ERROR: The certificate of is not trusted. 
解决方法：安装 ca-certificates 包
>$ sudo apt-get install ca-certificates

2 使用 --no-check-certificate 参数下载
># wget \-\-no-check-certificate https://www.python.org/ftp/python/3.5.1/Python-3.5.1.tgz


## sudo 命令没有权限

在  /etc/sudoers 文件中增加 用户权限

>abc ALL=(ALL:ALL) ALL

相关文档：http://man.linuxde.net/sudo

查看Python安装位置
-----
大家知道django是安装到python目录下的site-packages下的，但是这几个python目录下都没有site-packages这个文件夹，其实我们可以先通过下面的命令定位一下：

python -c "from distutils.sysconfig import get_python_lib; print get_python_lib()"

python3 -c "from distutils.sysconfig import get_python_lib; print (get_python_lib())"

sudo ln -s /opt/python-3.5.1/bin/python3.5 /usr/bin/python
sudo ln -s /opt/python-3.5.1/bin/pip3 /usr/bin/pip


## vim 设置

**语法高亮 **
> syntax on

** 制表符为4 **
> set tabstop=4 

** 统一缩进为4 **
> set softtabstop=4 
> set shiftwidth=4 


```
http://uwsgi-docs.readthedocs.org/en/latest/tutorials/Django_and_nginx.html
```

## 用户权限分配


### pip install 版本问题

```
root@sunroom:/home/abc# pip install virtualenv
Requirement already satisfied (use --upgrade to upgrade): virtualenv in /usr/local/python35/lib/python3.5/site-packages
You are using pip version 7.1.2, however version 8.0.3 is available.
You should consider upgrading via the 'pip install --upgrade pip' command.
```

需要升级pip版本

```pip install --upgrade pip```


### 测试图片地址

> http://pngimg.com/upload/butterfly_PNG1029.png

```
mkdir media
cd media 
wget http://pngimg.com/upload/butterfly_PNG1029.png
mv butterfly_PNG1029.png media.png
```


http://img2.3lian.com/img2007/14/03/20080405141042587.png

/var/run/mysite_nginx.sock

uwsgi --socket /var/run/mysite_nginx.sock --wsgi-file test.py


```
# mysite_uwsgi.ini file
[uwsgi]

# Django-related settings
# the base directory (full path)
chdir           = /home/abc/uwsgi/mysite
# Django's wsgi file
module          = mysite.wsgi
# the virtualenv (full path)
home            = /home/abc/uwsgi

# process-related settings
# master
master          = true
# maximum number of worker processes
processes       = 10
# the socket (use the full path to be safe
socket          = /home/abc/uwsgi/mysite/mysite.sock
# ... with appropriate permissions - may be needed
chmod-socket    = 666
# clear environment on exit
vacuum          = true

```

# create a directory for the vassals
sudo mkdir /etc/uwsgi
sudo mkdir /etc/uwsgi/vassals
# symlink from the default config directory to your config file
sudo ln -s /home/abc/uwsgi/mysite/mysite_uwsgi.ini /etc/uwsgi/vassals/
# run the emperor
uwsgi --emperor /etc/uwsgi/vassals --uid www-data --gid www-data


 /usr/local/python35/bin/uwsgi --emperor /etc/uwsgi/vassals --uid www-data --gid www-data --daemonize /var/log/uwsgi-emperor.log



 "
 /home/abc/www/bin/python3.5 -u -c "import setuptools, tokenize;__file__='/tmp/pip-build-d66t2tqy/Pillow/setup.py';exec(compile(getattr(tokenize, 'open', open)(__file__).read().replace('\r\n', '\n'), __file__, 'exec'))" install       --record /tmp/pip-d4liuly_-record/install-record.txt --single-version-externally-managed --compile --install-headers /      home/abc/www/include/site/python3.5/Pillow" failed with error code 1 in /tmp/pip-build-d66t2tqy/Pillow


### Pillow安装问题

pip install Pillow

目测你需要安装 python-dev用来编译一些 c 写的 python 库

pil 或者 pillow 想要正常工作，可能还需要 libjpeg-dev libpng-dev等依赖库

先安装依赖库，再重新安装 pillow 库。如果在线 pip 不能安装，可以使用离线的方式安装，下载pillow
源码，然后用 pip 安装 :pip install pillow--2.3.tar.gz


## nginx 安装包

###Installation

```
wget  https://github.com/centos-bz/ezhttp/archive/master.zip?time=$(date +%s) -O ezhttp.zip
unzip ezhttp.zip
cd ezhttp-master
chmod +x start.sh
./start.sh
```


### 安装打印结果信息

```
..
/usr/local/mysql/bin/mysqladmin: connect to server at 'localhost' failed
error: 'Access denied for user 'root'@'localhost' (using password: NO)'
Active Internet connections (only servers)
Proto Recv-Q Send-Q Local Address           Foreign Address         State       PID/Program name
tcp        0      0 0.0.0.0:3306            0.0.0.0:*               LISTEN      14212/mysqld
tcp        0      0 0.0.0.0:80              0.0.0.0:*               LISTEN      14020/nginx.conf
tcp        0      0 0.0.0.0:22              0.0.0.0:*               LISTEN      2143/sshd
tcp        0      0 127.0.0.1:25            0.0.0.0:*               LISTEN      2170/exim4
tcp6       0      0 :::22                   :::*                    LISTEN      2143/sshd
tcp6       0      0 ::1:25                  :::*                    LISTEN      2170/exim4
Active UNIX domain sockets (only servers)
Proto RefCnt Flags       Type       State         I-Node   PID/Program name    Path
unix  2      [ ACC ]     STREAM     LISTENING     5120     1830/acpid          /var/run/acpid.socket
unix  2      [ ACC ]     SEQPACKET  LISTENING     3108     304/udevd           /run/udev/control
unix  2      [ ACC ]     STREAM     LISTENING     40758    14212/mysqld        /usr/local/mysql/data/mysql.sock
```



### Django 1.8 install

pip install Django==1.8













注意红色加粗部分，需要将路径改为自己机器的相应路径。
接着，设置文件的访问权限：
chmod a+x /etc/init.d/nginx                                                         (a+x参数表示 ==> all user can execute  所有用户可执行)

chmod +x /etc/init.d/nginx


最后将ngix加入到rc.local文件中，这样开机的时候nginx就默认启动了
vim /etc/rc.local
添加
/etc/init.d/nginx start   
保存并退出
下次重启就会生效，实现nginx的自启动。





--------------

sudo chmod a+rwx -R logs   
sudo chmod a+rwx -R /usr/local/nginx 

nginx: [alert] could not open error log file: open() "/usr/local/nginx/logs/error.log" failed (13:Permission denied)
2014/08/04 20:35:45 [emerg] 17114#0: open() "/usr/local/nginx/logs/access.log" failed (13: Permission denied)
原因：当前用户对该位置没有写入权限
解决办法：
1.使用命令：sudo /usr/local/nginx/sbin/nginx -c /usr/local/nginx/conf/nginx.conf 以root权限启动
2.使用命令：sudo chmod -R a+rw /usr/local/nginx 给所有用户赋权限（个人学习，不考虑安全问题）
                    /usr/local/nginx/sbin/nginx -c /usr/local/nginx/conf/nginx.conf  启动Nginx

注：以非root权限启动时，会出现 nginx: [emerg] bind() to 0.0.0.0:80 failed (13: Permission denied) 错误
原因：Linux只有root用户可以使用1024一下的端口
解决办法：1.已root权限启动
  2.将 /usr/local/nginx/conf/nginx.conf 文件中的80端口改为1024以上
server {
# listen 80
   listen 8080
……
}