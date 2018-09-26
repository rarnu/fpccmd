# fpccmd

一个简单管理 CodeTyphon 项目的工具。

- - -

### 编译

使用 [CodeTyphon](http://www.pilotlogic.com/) (6.30 或以上版本) 来编译本项目。

请注意，如果你需要编译跨平台的版本，则需要在各个平台上均安装 CodeTyphon，而不是使用其本身的 Cross Element 来纺编译，以避免出现一些奇怪的问题。在本项目的 [Release](https://github.com/rarnu/fpccmd/releases) 页面内，已包含编译好的跨平台版本，可获取并使用之。

- - -

### 安装

Windows:

```
将 fpccmd.exe 放置到任意目录，并且在环境变量 PATH 中添加该目录。
```

Mac 和 Linux:

```
执行以下命令：
$ sudo cp fpccmd /usr/local/bin/
```

不论在哪个平台进行安装，都必须保证 ```git``` 的安装完好，并且 git 命令已被配置入 PATH 环境变量内。 

- - -

### 使用(1) 编译项目

要使用 ```fpccmd``` 来编译项目，必须先进行初始化，执行以下命令：

```
$ fpccmd init
```

执行完毕后，项目内会出现 ```fpccmd.cfg```，```fpccmd.before```，```fpccmd.after``` 三个文件。

```fpccmd.cfg``` 用于进行额外的 fpc 参数配置，并且已包含了基础的配置选项，这些选项按每一行一个进行设置。

注意，对于搜索目录的配置，若被搜索的目录在项目内，则不需要进行配置，fpccmd 会自动进行管理。若搜索目录在项目外，则需要进行配置，例如 ```-Fu../../myct/util```。

```fpccmd.before``` 和 ```fpccmd.after``` 用于在编译前后执行特定命令，命令的写法如下：

```
${平台}${UI类型} 命令
例如：
${ALL}${UALL} cp ${PATH}/a.png ${PATH}/${PROJ}/image/a.png  # 在所有平台，所有 UI 类型下，执行 cp 命令来复制图片
${M}${UNA} cp ${PATH}/libsample.dylib ${PATH}/${PROJ}.app/Contents/MacOS/libsample.dylib # 在 Mac 平台，不使用 UI 框架的情况下，执行 cp 命令来复制动态库
```

在 before 和 after 文件内，可用的命令有以下几种（全部遵循 linux 的命令风格，但是对于目录之类的不需要加参数，程序会自动处理）：

```
cp:    复制文件或目录
mv:    移动文件或目录
rm:    删除文件或目录
mkdir: 创建文件夹（可以带子文件夹）
```

在配置完成后，可以执行以下命令来编译一个项目：

```
$ fpccmd <目标平台> <主程序文件> <UI 类型>
例如：
$ fpccmd M sample.ppr C  # 编译 Mac 下的应用，采用 Cocoa UI 框架
$ fpccmd L sample.ppr G  # 编译 Linux 下的应用，采用 GTK2 UI 框架
$ fpccmd AA sample.ppr   # 编译 Android(arm) 下的应用，不采用 UI 框架
```

- - -

### 使用(2) 依赖模块管理

fpccmd 可以帮助管理 CodeTyphon 项目所使用的模块。

要使用 fpccmd 来管理模块，必须先编写一个 ```fpcdep.spec``` 文件，该文件描述了要依赖的模块。

可以通过以下命令来新建一个 ```fpcdep.spec```：

```
$ fpccmd module initdep
```

然后编写 ```fpcdep.spec``` 的内容：

```
source=https://github.com/rarnu/
myct,1.0
```

第一行标出了要使用的源，第二行标出了要使用的具体模块名称和 git tag。

如果有多个源或多个依赖项，可以把 spec 文件写成以下形式：

```
source=https://github.com/rarnu/
myct,1.0
samplemodule,0.2.0

source=https://rarnu.xyz:8080/
crossorca,0.5.1
```

配置完成后，可以执行以下命令，来自动获取所依赖的模块，并自动修改工程(.ctpr)文件：

```
$ fpccmd module get
```

根据依赖模块的大小和网络情况，需要不同的下载时间，耐心等待即可。

若是觉得全模块的下载过于费时，也可以只下载指定的模块，在命令后加入模块名称即可，如：

```
$ fpccmd module get myct  # 此时只下载并配置 myct 这个模块
```

你也可以把自己的 FPC 模块做成共享的，以便他人使用，fpccmd 不要求有一个中央模块仓库，仅使用 github（或其他的 git 平台） 即可。

要共享一个 FPC 模块，你需要拥有一个 ```fpcmodule.spec``` 文件，使用以下命令可以创建一个：

```
$ fpccmd module initspec
```

此时将得到一个 ```fpcmodule.spec``` 文件，按文件内的注释进行填写，最终 push tag 到自己的 git 仓库即可完成模块共享。

一个合法的 ```fpcmodule.spec``` 应当是如下所示的：

```
url=https://github.com/rarnu/myct.git
version_code=1
version_tag=1.0
author=rarnu
module_name=myct
unit_dirs=orca;util;thread
include_dirs=
library_dirs=
required_package=adLCL;pl_orca
```

- - -

### 使用(3) 配置 Mac 端 iOS 开发环境

对于需要进行 iOS 跨平台开发的人员来说，必须事先配置 iOS 端的环境。

fpccmd 提供了在 Mac 上直接进行配置的能力，只需要执行以下命令：

```
$ sudo fpccmd install-ios
```

这将为你下载将近 600M 的文件，若是觉得远程服务器太慢，也可以自行更换下载源，使用以下代码即可：

```
$ fpccmd set-source <源URL>
```

若要查看当前的下载源，可以使用以下命令：

```
$ fpccmd source
```

对于被制作成下载源的服务器，要求在其路径下，包含名为 ```fpc_ios.tar.gz``` 的文件，即一个完整的 iOS 跨平台工具链。

例如设置的源是 ```http://120.27.9.223/repo/```，则文件的路径必须为 ```http://120.27.9.223/repo/fpc_ios.tar.gz```，此时 fpccmd 可以正确的进行 iOS 环境的配置。
