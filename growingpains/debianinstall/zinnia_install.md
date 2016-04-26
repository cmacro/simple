
# 部署 blog

## 下载并安装虚拟机

Debian可以直接到官网下载。还有种比较快的下载是在国内的镜像上下载，`163`和`sohu`的镜像基本都是满带宽的速度。

>
mirrors.163.com   #163 镜像
mirrors.sohu.com  #sohu 镜像 （Debian的版本比较全）


系统安装文件放在`debian-cd`目录中，选择需要的版本下载。

### 虚拟机

`VisualBox`是开源免费的这肯定是首选，如果你已经购买过其他商业虚拟机，这节可以略过。

**VisualBox官网** [www.virtualbox.org](https://www.virtualbox.org)



### 安装 pip

>
sudo apt-get install vim -y
sudo apt-get install python-dev -y
sudo apt-get install python-pip -y
pip install virtualenv

sudo apt-get install python-dev -y

升级 pip
pip install --upgrade pip


/home/abc/www/mysite

sudo ln -s //mysite/mysite_uwsgi.ini /etc/uwsgi/vassals/


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
    server_name localhost; # substitute your machine's IP address or FQDN
    charset     utf-8;

    # max upload size
    client_max_body_size 75M;   # adjust to taste

    # Django media
    location /media  {
        alias /home/abc/www/mysite/media;  # your Django project's media files - amend as required
    }

    location /static {
        alias /home/abc/www/mysite/static; # your Django project's static files - amend as required
    }

    # Finally, send all non-media requests to the Django server.
    location / {
        uwsgi_pass  django;
        include    /home/abc/www/mysite/uwsgi_params; # the uwsgi_params file you installed
    }
}
```