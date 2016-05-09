
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




## 编译部署NGINX

刚写完这篇文章就出了[nginx-1.10.0](http://nginx.org/)的稳定版本。小伙伴们可以升级了。

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

若安装时找不到上述依赖模块，使用
```
--with-openssl=<openssl_dir>
--with-pcre=<pcre_dir>
--with-zlib=<zlib_dir>
```
指定依赖的模块目录。如已安装过，此处的路径为安装目录；若未安装，则此路径为编译安装包路径，nginx将执行模块的默认编译安装。


### 增加站点启动配置

在nginx.conf中增加包含配置路径，这样每次启动nginx自动会加载目录中的所有配置。

这个方法是方便，再一个服务器上部署多站点点时相当方便。

由于是编译安装，全新的服务器是没有相应的目录，需要自己创建。
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


> **说明：**   
> 为什么把路径设置到 /etc/nginx/sites-enabled。  
> 是一般习惯的做法，使用apt-get 包安装方法就会放在这个地府。当然也可以放在其他地方，为维护方便减少记忆使用标准的会比较容易找。  




### 启动Nginx

安装完毕后nginx没有启动，需要手工启动

```
$ sudo /usr/local/nginx/sbin/nginx -c /usr/local/nginx/conf/nginx.conf
```

启动nginx之后，浏览器中输入http://192.168.10.14 可以验证是否安装启动成功。看到欢迎页面那就恭喜你OK了。

上面还有个问题，每次启动系统或维护打这么一长串命令不方便。因此还是要和安装包那种一样，启动系统直接启动和增加维护命令。

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


#### 安装uwsgi

```
$ pip install uwsgi
```

#### 创建一个测试文件 test.py

```
# test.py
def application(env, start_response):
    start_response('200 OK', [('Content-Type','text/html')])
    return [b"Hello World"] # python3
    #return ["Hello World"] # python2
```


#### 运行uWSGI

```
$ uwsgi --http :8000 --wsgi-file test.py
```

> **选项：**  
> http:8000  使用8000端口访问  
> wsgi-file test.py 加载刚新建的测试文件test.py

在浏览器中测试，正常应该可以在浏览器中看到 `hello world`。

```http://192.168.10.14:8000```


这样，uWSGI就调通了。可以继续下一步Django工程的测试

> **处理流程：**  
> 客户端 <-> uWSGI <-> python  


#### Django工程测试

刚才已经新建了一个mysite的Django工程，可以直接运行这个站点。前面的方法是运行单个python文件的方法，运行站点的方法稍微有些不同。

先测试一下mysite，确保没问题

```
$ python manage.py migrate      
$ python manage.py runserver 0.0.0.0:8000

```

在浏览器上就能看到下面内容，就说明OK了

```
It worked!
Congratulations on your first Django-powered page.
...
```

使用uWSGI运行站点。

```uwsgi --http :8000 --module mysite.wsgi```


> **访问流程：**  
> client <-> uWSGI <-> Django  


### nginx基础

#### 使用Nginx配置站点

正常访问uwsgi需要一个`uwsgi_params`的文件，在编译安装的目录中有这个文件，可以直接使用。如果没有可以从 https://github.com/nginx/nginx/blob/master/conf/uwsgi_params 获取

把这个文件复制到mysite工程目录中。

```
$ cp /usr/local/nginx/conf/uwsgi_params ./
```

增加2个目录`media`和`static`, Django会使用到。

```
$ mkdir media
$ mkdir static
```


创建一个站点配置文件 `mysite_nginx.conf`

> **注意：**   
> /home/abc/uwsgi-tutorial/mysite   
> 这个工程目录如有不同，需要修改一下。可以使用`pwd`查看你当前的目录   
>


```
$ cat >mysite_nginx.conf
```

添加下面内容

```
# mysite_nginx.conf

# the upstream component nginx needs to connect to
upstream django {
    # server unix:///home/abc/uwsgi-tutorial/mysite/mysite.sock; # for a file socket
    server 127.0.0.1:8001; # for a web port socket (we'll use this first)
}

# configuration of the server
server {
    # the port your site will be served on
    listen      8000;
    # the domain name it will serve for
    server_name localhost; # substitute your machine's IP address or FQDN
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

把配置文件映射到nginx的站点配置启动目录`sites-enabled`中。先前nginx配置时增加的那个目录。


配置文件映射到 ```/etc/nginx/sites-enabled/```

```
$ sudo ln -s /home/abc/uwsgi-tutorial/mysite/mysite_nginx.conf /etc/nginx/sites-enabled/
```


重启nginx服务

```
sudo /etc/init.d/nginx restart
```

#### 部署静态文件

在启动nginx之前，需要把Django的静态文件生成到`static`静态目录中。需要改一下`mysite/settings.py`的配置，在最后增加一行。

```
STATIC_ROOT = os.path.join(BASE_DIR, "static/")
```

```
python manage.py collectstatic
```


#### 测试nginx

重启nginx

```
$ sudo /etc/init.d/nginx restart
```

在media目录中增加一个media.png文件，测试一下nginx是否正常工作。

http://192.168.10.14:8000/media/5.png


> **提示：**可以从网上下载一个png文件复制到目录中，用wget下载。

```
wget https://raw.githubusercontent.com/cmacro/simple/master/other/2.png
```

如果没看到图片，可能会有一下情况。

> **问题排查：**  
> 1、重启Nginx  
> 2、配置文件没有启动，看`nginx.conf` 是否包含了 `/etc/nginx/sites-enabled/`  
> 3、配置文件中的工程路径有问题，`mysite_nginx.conf` 中配置的目录是不是mysite工程的目录。  


#### 使用nginx+uWSGI+test.py测试

```
uwsgi --socket :8001 --wsgi-file test.py
```

正常情况下`http://192.168.10.14:8000`能看到 `Hello World`

这个和原来的简单测试uWSGI的方法有些差异，原来是使用 8000端口，现在使用的是8001端口。在访问8000端口时能看到说明nginx正常工作。

> **解析流程：**  
> client <-> nginx <-> socket <-> uWSGI <-> Python

上述这种方式，比较简单。还有中方式是直接使用unix的套接字，能减少系统开销。

修改原来的配置文件`mysite_nginx.conf`, 第一行注释去掉，注释第二行

```
    # server unix:///home/abc/uwsgi-tutorial/mysite/mysite.sock; # for a file socket
    server 127.0.0.1:8001; # for a web port socket (we'll use this first)
```

重新启动nginx，运行uWSIG。
```
uwsgi --socket mysite.sock --wsgi-file test.py
```

访问 `http://192.168.10.14:8000`。 可能看不到内容，nginx没正常执行，这种情况一般是权限问题。

可以看nginx的错误日志。(` /usr/local/nginx/logs/error.log`),
```
 connect() to unix://home/abc/uwsgi-tutorial/mysite/mysite.sock failed (13: Permission denied)
```

增加权限执行
```
uwsgi --socket mysite.sock --wsgi-file test.py --chmod-socket=666 # (very permissive)
or:
uwsgi --socket mysite.sock --wsgi-file test.py --chmod-socket=664 # (more sensible)
```

这样就能看到熟悉的`hello world`


#### 在使用nginx和uWSGI执行Django应用

```
uwsgi --socket mysite.sock --module mysite.wsgi --chmod-socket=666
```

现在可以通过uWSGI和nginx提供的服务看到Django工程的Hello world。

### 使用配置.ini运行uWSGI

上面一堆参数命令，每次敲肯定比较麻烦。uWSGI可以使用配置文件执行运行。

创建一个 'mysite_uwsgi.ini' 内容如下

```
$ cat >mysite_uwsgi.ini
```

```
# mysite_uwsgi.ini file
[uwsgi]

# Django-related settings
# the base directory (full path)
chdir           = /home/abc/uwsgi-tutorial/mysite
# Django's wsgi file
module          = mysite.wsgi
# the virtualenv (full path)
home            = /home/abc/uwsgi-tutorial

# process-related settings
# master
master          = true
# maximum number of worker processes
processes       = 10
# the socket (use the full path to be safe
socket          = /home/abc/uwsgi-tutorial/mysite/mysite.sock
# ... with appropriate permissions - may be needed
chmod-socket    = 666
# clear environment on exit
vacuum          = true

```

运行配置文件,这样简单清爽多了。

```
$ uwsgi --ini mysite_uwsgi.ini
```

## 部署到系统

刚才测试部署的都是在虚拟环境virtualenv中配置运行，需要投入到运行环境中。


退出virtualenv并安装uWSGI

```
$ deactivate
$ sudo pip install uwsgi
```

运行,看到效果就OK啦

```
uwsgi --ini mysite_uwsgi.ini
```





## 后续

Nginx + uWSGI + Django 的部署基本完成。自己编译安装，实际还是挺麻烦的。光整理这个过程都花了好长时间。



> **相关内容：**  
> 授权命令chown可以参考[**linux权限命令chown**](http://www.moguf.com/post/linuxcmdchown)  说明
> 


### debing国内服务器镜像

#### 网易163更新服务器：

```
deb http://mirrors.163.com/debian/ squeeze main non-free contrib
deb http://mirrors.163.com/debian/ squeeze-proposed-updates main non-free contrib
deb-src http://mirrors.163.com/debian/ squeeze main non-free contrib
deb-src http://mirrors.163.com/debian/ squeeze-proposed-updates main non-free contrib
```

#### sohu 更新服务器：

```
deb http://mirrors.sohu.com/debian/ lenny main non-free contrib
deb http://mirrors.sohu.com/debian/ lenny-proposed-updates main non-free contrib
deb-src http://mirrors.sohu.com/debian/ lenny main non-free contrib
deb-src http://mirrors.sohu.com/debian/ lenny-proposed-updates main non-free contrib
```


### 查找当前python版本的库目录

python3 和 python2 改一下print

```
$ python -c "from distutils.sysconfig import get_python_lib; print (get_python_lib())"
```



### 下载wget出现无效证书错误

wget 下载时出现 Wget error: ERROR: The certificate of is not trusted. 
**解决方法：**安装 `ca-certificates` 包
```
$ sudo apt-get install ca-certificates
```

2 使用 --no-check-certificate 参数下载
```
$ wget \-\-no-check-certificate https://www.python.org/ftp/python/3.5.1/Python-3.5.1.tgz
```


### sudo 命令没有权限

在  /etc/sudoers 文件中增加 用户权限

```
abc ALL=(ALL:ALL) ALL
```


### vim 设置


说明 | 语法
---| ---
语法高亮 | syntax on
制表符为4 | set tabstop=4
统一缩进为4 | set softtabstop=4
自动缩进 | set shiftwidth=4



### pip install 版本问题

```
root@sunroom:/home/abc# pip install virtualenv
Requirement already satisfied (use --upgrade to upgrade): virtualenv in /usr/local/python35/lib/python3.5/site-packages
You are using pip version 7.1.2, however version 8.0.3 is available.
You should consider upgrading via the 'pip install --upgrade pip' command.
```

需要升级pip版本

```
$ sudo pip install --upgrade pip
```


### 安装Django指定版本

如安装 1.8 版本的

```
$ sudo pip install Django==1.8
```

