
# git私有服务器搭建

## 安装git

直接通过apt-get安装git

```
# apt-get update
# apt-get install git -y
```

## 创建git用户

```
# mkdir /home/git
# useradd git -d /home/git -s /usr/bin/git-shell
```



相关文档
http://beiyuu.com/vps-config-python-vitrualenv-flask-gunicorn-supervisor-nginx/

##添加用户bob
##参数-d：指定用户目录
##参数-m：如果目录不存在则创建
##参数-s：只用用户使用的 shell

> # useradd myweb -d /home/myweb -m -s /bin/bash

useradd git -d /home/git -m -s /usr/bin/git-shell




