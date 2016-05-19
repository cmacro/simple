
## 一.自动化安装脚本：


> https://github.com/centos-bz/EZHTTP



> https://github.com/centos-bz/ezhttp/archive/master.zip

### Installation
```
wget https://github.com/centos-bz/ezhttp/archive/master.zip?time=$(date +%s) -O ezhttp.zip
unzip ezhttp.zip
cd ezhttp-master
chmod +x start.sh
./start.sh
```

### Process Management

Process	Command
nginx	/etc/init.d/nginx (start|stop|restart)
apache	/etc/init.d/httpd (start|stop|restart)
php-fpm	/etc/init.d/php-fpm (start|stop|restart)
mysql	/etc/init.d/mysqld (start|stop|restart)

#### ez command description

Command	Description
ez vhost add	create virtual host
ez vhost list	list all virtual host
ez vhost del	remove a virtual host
ez mysql reset	reset mysql password
ez mysql add	create a mysql user



推荐选择安装（nginx,mysql-5.5）

## MySQL

### MySQL 分配用户和权限


//登录MYSQL
@>mysql -u root -p
@>密码
//创建用户

```
mysql> insert into mysql.user(Host,User,Password) values('localhost','myweb',password('myweb'));
```

```
mysql> create database myweb character set utf8;

insert into mysql.user(host, user, password) values('localhost', 'myweb', password('myweb'));
grant all  on myweb.* to myweb@localhost identified by 'myweb';
flush privileges;
```


## 二.node.js安装：

```
wget https://nodejs.org/dist/v0.12.7/node-v0.12.7-linux-x64.tar.gz
wget https://nodejs.org/dist/v0.12.7/node-v0.12.7-linux-x86.tar.gz

tar zxvf node-v0.12.7-linux-x86.tar.gz
mv node-v0.12.7-linux-x86 /usr/local/node
ln -s /usr/local/node/bin/ /usr/bin/node

ln -s /usr/local/node/bin/node /usr/bin/node
ln -s /usr/local/node/bin/npm /usr/bin/npm


cd ~
vi ./profile
export PATH=$PATH:/usr/local/node/bin
source ./profile
node -v //出现v0.12.7就好了
```


## 三.安装pm2管理node服务：
npm install pm2 -g 如果很慢就取消先配置淘宝镜像：（ npm config set registry https://registry.npm.taobao.org）

```
npm config set registry https://registry.npm.taobao.org
npm install pm2 -g
```

到此所有环境结束只要等网站弄好，把nginx代理搞一下就好


wget https://github.com/cmacro/NEMBlog/archive/master.zip
unzip master.zip
sudo mv NEMBlog-master /home/blog
cd /home/blog
sudo npm install
node index

sudo npm install compression


<<<<<<< HEAD
ez


```
Package: lnmp

*****Nginx Setting*****
Nginx: nginx-1.8.0
Nginx Location: /usr/local/nginx
Nginx Configure Parameter: --prefix=/usr/local/nginx --with-http_ssl_module --with-openssl=/home/abc/ezhttp-master/soft/openssl-1.0.2f  --with-http_sub_module --with-http_stub_status_module --with-pcre --with-pcre=/home/abc/ezhttp-master/soft/pcre-8.33 --with-zlib=/home/abc/ezhttp-master/soft/zlib-1.2.8 --with-http_secure_link_module
Nginx Modules: do_not_install


*****MySQL Setting*****
MySQL Server: mysql-5.1.73
MySQL Location: /usr/local/mysql
MySQL Data Location: /usr/local/mysql/data
MySQL Port Number: 3306
MySQL Root Password: sqm718
MySQL Configure Parameter: --prefix=/usr/local/mysql --sysconfdir=/usr/local/mysql/etc --with-unix-socket-path=/usr/local/mysql/data/mysql.sock --with-charset=utf8 --with-collation=utf8_general_ci --with-extra-charsets=complex --with-plugins=max --with-mysqld-ldflags=-all-static --enable-assembler 

*****PHP Setting*****
PHP: do_not_install

*****Other Software Setting*****
Other Software: do_not_install

```
=======






// 进程运行
pm2 start index.js

// 显示列表
pm2 list

// 关闭
pm2 stop index
或
pm2 kill




查看字符集设置
mysql> show variables like 'collation_%';

mysql> show variables like 'character_set_%';

修改数据库的字符集
mysql>use mydb
mysql>alter database mydb character set utf8;
创建数据库指定数据库的字符集
mysql>create database mydb character set utf8;
 
 
通过配置文件修改:
修改/var/lib/mysql/mydb/db.opt
default-character-set=latin1
default-collation=latin1_swedish_ci
为
default-character-set=utf8
default-collation=utf8_general_ci
重起MySQL:
[root@bogon ~]# /etc/rc.d/init.d/mysql restart




http://192.168.103.157:3000/admin/install

//登录MYSQL
@>mysql -u root -p
@>密码
//创建用户
mysql> insert into mysql.user(Host,User,Password) values('localhost','nemblog’,password(‘nemblog’));

insert into mysql.user(host, user, password) values('localhost', 'nemblog', password('nemblog'))

//刷新系统权限表
mysql>flush privileges;
这样就创建了一个名为：jeecn  密码为：jeecn  的用户。

//退出后登录一下
mysql>exit;
@>mysql -u jeecn -p
@>输入密码
mysql>登录成功

2.为用户授权


查看授权
show grants for 你的用户;
show grants for root@'localhost'; 
show grants for webgametest@10.3.18.158;
show create database dbname;  这个可以看到创建数据库时用到的一些参数。 
show create table tickets;    可以看到创建表时用到的一些参数

//登录MYSQL（有ROOT权限）。我里我以ROOT身份登录.
@>mysql -u root -p
@>密码
//首先为用户创建一个数据库(jeecnDB)
mysql>create database jeecnDB;
//授权jeecn用户拥有jeecn数据库的所有权限
@>grant all  on jeecnDB.* to jeecn@localhost identified by ‘jeecn’;
//刷新系统权限表
mysql>flush privileges;
mysql>其它操作

//如果想指定部分权限给一用户，可以这样来写:
mysql>grant select,update on jeecnDB.* to jeecn@localhost identified by ‘jeecn’;
//刷新系统权限表。
mysql>flush privileges;

mysql> grant 权限1,权限2,…权限n on 数据库名称.表名称 to 用户名@用户地址 identified by ‘连接口令’;

权限1,权限2,…权限n代表select,insert,update,delete,create,drop,index,alter,grant,references,reload,shutdown,process,file等14个权限。
当权限1,权限2,…权限n被all privileges或者all代替，表示赋予用户全部权限。
当数据库名称.表名称被*.*代替，表示赋予用户操作服务器上所有数据库所有表的权限。
用户地址可以是localhost，也可以是ip地址、机器名字、域名。也可以用’%’表示从任何地址连接。
‘连接口令’不能为空，否则创建失败。

例如：
mysql>grant select,insert,update,delete,create,drop on vtdc.employee to jee@10.163.225.87 identified by ‘123′;
给来自10.163.225.87的用户jee分配可对数据库vtdc的employee表进行select,insert,update,delete,create,drop等操作的权限，并设定口令为123。

mysql>grant all  on vtdc.* to jee@10.10.10.87 identified by ‘123′;
给来自10.163.225.87的用户jee分配可对数据库vtdc所有表进行所有操作的权限，并设定口令为123。

mysql>grant all  on *.* to jee@10.10.10.87 identified by ‘123′;
给来自10.163.225.87的用户jee分配可对所有数据库的所有表进行所有操作的权限，并设定口令为123。

mysql>grant all privileges on *.* to jee@localhost identified by ‘123′;
给本机用户jee分配可对所有数据库的所有表进行所有操作的权限，并设定口令为123。

3.删除用户

@>mysql -u root -p
@>密码
mysql>DELETE FROM user WHERE User=”jeecn” and Host=”localhost”;
mysql>flush privileges;
//删除用户的数据库
mysql>drop database jeecnDB;

4.修改指定用户密码

@>mysql -u root -p
@>密码
mysql>update mysql.user set password=password(‘新密码’) where User=”jeecn” and Host=”localhost”;
mysql>flush privileges;
mysql>quit;

顶





ez
>>>>>>> 5f3121bee39cdd225514a4c5c083fab8e7e1d41b

## 设置nodejs的nginxd

记录一下，如何配置nodejs nginx的反向代理
本文是在mac下配置nodejs 在nginx下的反向代理

1.安装nodejs，之前就安装了。

2.安装nginx ，我采用的直接源码安装

3.进入 /usr/local/nginx/conf 目录，在该目录下创建include 文件下，我的配置文件就写在这个文件夹里面

4.进入 /usr/local/nginx/conf/include 目录，创建 nginx.node.conf 文件，在里面输入如下代码：

复制代码
upstream nodejs_nemblog {
    server 127.0.0.1:3000;
    keepalive 64;
}

server {
    listen 80;
    server_name www.moguf.com moguf.com;
    access_log /var/log/nginx/.log;
    location / {
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header Host  $http_host;
        proxy_set_header X-Nginx-Proxy true;
        proxy_set_header Connection "";
        proxy_pass      http://nodejs_nemblog;
    }
}

复制代码
5.进入/usr/local/nginx/conf ，打开nginx.conf, 在http 里面添加 include /usr/local/nginx/conf/include/*

6.重启nginx , 输入 /usr/local/nginx/sbin/nginx -c /usr/local/nginx/conf/nginx.conf

在浏览器输入penguu.com ok.



mysql -uroot -p dbname < bakfile 


