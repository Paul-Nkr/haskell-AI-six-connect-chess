（a）提交代码包括 AIchess 实现HW3的全功能，包含AI与IO部分
（b）程序需要importHaskell内建库 具体如下
	import Data.List
	import System.IO
	import System.Directory
	import Control.Concurrent
	import Data.Time.Clock
（c）编译方法有两种
一：与其余hs程序一致
在terminal内先使用ghci进入haskell环境
使用 :l AIchess.hs 进行编译
使用main呼出主函数
此时根据提示输入 0 1确定该terimal落子颜色 注意若本方为黑色 白方应在10S（现修改为2min）内开启 否则将视作超时
二：打开两个terminal，执行ghc --make AIchess生成可执行文件（本程序环境是在虚拟机中，其中AIchess是程序名），
然后两个terminal分别执行./AIchess,得到提示输入0或1；一边输入0，一边输入1，下棋开始。