#!/usr/bin/python
# _*_ utf-8 _*_

import os
def searchDirBuild(rootpath, splitlen, prefix, incwords, excwords):
    """
    编译搜索目录生成
    """
    searchpath = ''    
    for root, dirs, files in os.walk(rootpath):
        for name in dirs:
            isout = False
            #print(name)
            if name in incwords:
                isout = True

            #print(name , True)
            for n in incwords:
                if root.find(n) > 0:
                    isout = True
                    continue

            if isout:
                if name in excwords:
                    isout = False
                else:
                    for n in excwords:
                        if root.find(n) > 0:
                            isout = False
                            continue                        

            if isout:
                if prefix != "":
                    scrpath = prefix + root[splitlen:] + '\\' + name
                else :
                    scrpath = root + '\\' + name
                searchpath += ';' + scrpath
                print(scrpath)

    return searchpath[1:]


#invalidatenames = {'Source', 'debug', 'release', '.svn', 'Icons', 'Release', 'Debug', 'source', 'Tools', 'Region'}
invalnames = {'Region', 'demos'}
validatenames = {'Include', 'include'}
rootpath = r'D:\Q5\core'
pathlen = len(rootpath) - 4
earchpath = searchDirBuild(rootpath, pathlen, '..\\..\\..\\', validatenames, invalnames)
#print(earchpath)



def renameFileNameinDir(rootpath, oldpf, newpf):
    """
    批量修改文件名前缀
    """
    count = 0
    oldlen = len(oldpf)
    for root, dirs, files in os.walk(rootpath):
        for name in files:
            if len(name) > oldlen and name[:oldlen] == oldpf:
                srcname =  root + '\\' + name
                destname = root + '\\' + newpf + name[oldlen:]
                os.rename(srcname, destname)
                count += 1
                print('rename:' + root + '  ' + name + '   ===>  ' + newpf + name[oldlen:])
    return count

#rootpath = r'd:\downloads\nginxsrc\nginxsrc'
#renamecount = renameFileNameinDir(rootpath, 'ngx_', 'ml')
#print('renname file count: ', renamecount)


def checkSamefileNames(rootpath):
    """
    搜索同名文件
    """
    count = 0
    filedict = {}
    for root, dirs, files in os.walk(rootpath):
        for name in files:
            if name in filedict:
                count += 1;
                print ('same file:' + name + '\t\t' + filedict[name] + '\t<===>\t' + root)
            else :
                filedict[name] = root

            #print(name)

    return filedict


##rootpath = r'D:\devs\opensource\ml'
#filedict = checkSamefileNames(rootpath)
#print('----------------------')
#print('file count: ' ,len(filedict))




#.\core;.\doc;.\ext;.\os;.\other;.\plugin;.\thr;.\ui;.\os\win32;.\plugin\__pycache__;.\ui\gtk;.\ui\osx;.\ui\win

#..\..\src\core;..\..\src\doc;..\..\src\ext;..\..\src\os;..\..\src\other;..\..\src\plugin;..\..\src\thr;..\..\src\ui;..\..\src\os\win32;..\..\src\ui\gtk;..\..\src\ui\osx;..\..\src\ui\win