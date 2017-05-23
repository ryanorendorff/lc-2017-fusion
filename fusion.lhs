%options ghci -fglasgow-exts

%if False

> {-# OPTIONS -XStandaloneDeriving -XFlexibleInstances -XFlexibleContexts #-}
> {-# LANGUAGE BangPatterns #-}
> module Fusion where
>
> import Prelude
> import qualified Data.Vector as V

%endif

\documentclass[]{beamer}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt
%include beamer.fmt

\usepackage{appendixnumberbeamer}
\usepackage{booktabs}
\usepackage{minted}

\usetheme{metropolis}
\definecolor{BerkeleyBlue}{RGB}{0,50,98}
\definecolor{FoundersRock}{RGB}{59,126,161}
\definecolor{Medalist}{RGB}{196,130,14}
\setbeamercolor{frametitle}{fg=white,bg=FoundersRock}
\setbeamercolor{title separator}{fg=Medalist,bg=white}
% \usefonttheme[onlymath]{serif}
\usefonttheme[onlymath]{serif}

\usepackage[macros]{acro}
\acsetup{short-format = \scshape}

\DeclareAcronym{adt}{short = adt, long = algebraic data type}

\usepackage{amsmath,dsfont}
\usepackage{xspace}

\newcommand{\vpause}{\vspace*{-\baselineskip}\pause}

\newcommand{\rnote}[1]{\color{red} #1}

%%format bottom = "\perp"
%
%%format Bool = "\bools"
%%format Set = "\sets"
%%format && = "\wedge"
%%format || = "\vee"

\AtBeginSection[]
{
  \begin{frame}
    \frametitle{Table of Contents}
    \tableofcontents[currentsection]
  \end{frame}
}

\title{Deforestation and Program Fusion: Applying Equational Transforms to Automatically Simplify Programs}
\subtitle{\verb|github.com/ryanorendorff/xxxxxxxxxxxxx|}
\author{Ryan~Orendorff, PhD?\inst{1}}

\institute{
  \inst{1}%
  Department of Bioengineering\\
  University of California, Berkeley\\
  University of California, San Francisco
}
\date{May 2017}

\begin{document}

\frame{\titlepage}

\section{Motivation: Simple Programs versus Performance}

%format foldr0 = "foldr"
%format map0 = "map"
%format sum0 = "sum"

\begin{frame}
\frametitle{Common way to process a list: map and fold!}

As an example, say we want to square all the elements in a list and then sum
the result.

> process :: [Int] -> Int
> process xs = sum0 . map0 sq $ xs

Where we have defined the functions as follows.


> map0  _  []      = []
> map0  f  (x:xs)  = f x : map0 f xs
>
> sq x = x * x

\pause

> foldr0  _  b  []      =  b
> foldr0  f  b  (a:as)  =  foldr0 f (f a b) as
>
> sum0 = foldr0 (+) 0

\end{frame}


\begin{frame}
\frametitle{How fast is |process|?}

So now that we have our process function, how fast does it run?

< process :: [Int] -> Int
< process xs = sum0 . map0 sq $ xs

Let's try to process a million elements with our |process| and |process'|,
which uses the standard Prelude |sum| and |map|.

%if False

> process' :: [Int] -> Int
> process' xs = sum . map sq $ xs

%endif

%format million = "1,000,000"

< process   [0..million]; process'  [0..million]

\pause

\vspace{-1.5em}

\begin{center}
\begin{tabular}{rcc}
\toprule
  Function & Time (ms) & Memory (MB) \\
  \midrule
  |process|       & 220.0 & 265.26 \\
  |process'|      & 5.1   & 80.00\\
  \bottomrule
\end{tabular}
\end{center}

\pause

\emph{How does the Prelude do so much better with the same functions?}

\end{frame}


%format processFused' = "processFused"

%if False

\begin{frame}
\frametitle{We can get good performance with manual code}

We can try to get better performance by writing our program as a recursive
function.

< process :: [Int] -> Int
< process xs = sum0 . map0 sq $ xs

\pause

> processFused :: [Int] -> Int
> processFused []     = 0
> processFused (x:xs) = x * x + processFused xs

\pause

\vspace{-1.5em}

\begin{center}
\begin{tabular}{rc}
\toprule
  function & time (ms) \\
  \midrule
  |process|       & 220.0 \\
  |process'|      & 5.1   \\
  |processFused|  & 26.8  \\
  \bottomrule
\end{tabular}
\end{center}

We are closer, but still not quite to GHC level performance.


\end{frame}

%endif

\begin{frame}[fragile]
\frametitle{Let's try to optimize with an accumulator}

What if we try to optimize |process| using an accumulator?

< process :: [Int] -> Int
< process xs = sum0 . map0 sq $ xs

\pause

This means we make a tail-recursive "loop".

> processFused' :: [Int] -> Int
> processFused' = loop 0
>   where
>     loop n  []      = n
>     loop n  (x:xs)  = loop (n + x * x) xs

\pause

\vspace{-1.5em}

This mirrors the version of the program one would write imperatively.

\end{frame}

\begin{frame}
\frametitle{How well does our manually fused version do?}

< process xs = sum0 . map0 sq $ xs
< process' xs = Prelude.sum . Prelude.map sq $ xs
<
< processFused' :: [Int] -> Int
< processFused' = loop 0
<   where
<     loop n  []      = n
<     loop n  (x:xs)  = loop (n + x * x) xs

\pause
\vspace{-1.5em}

\begin{center}
\begin{tabular}{rcc}
\toprule
  Function & Time (ms) & Memory (MB) \\
  \midrule
  |process|       & 220.0 & 265.26 \\
  |process'|      & 5.1   & 80.00\\
 %|processFused|  & 26.8  & 96.65\\
  |processFused'| & 4.7   & 80.00 \\
  process.c       & 2.6   & $8\times10^{-5}$ \\
  \bottomrule
\end{tabular}
\end{center}

\end{frame}

\begin{frame}
\frametitle{GHC generated the loop version automatically}

Our manual version |processFused'|.

< processFused' :: [Int] -> Int
< processFused' = loop 0
<   where
<     loop n  []      = n
<     loop n  (x:xs)  = loop (n + x * x) xs

and when we compile the Prelude defined |process'|, GHC produces

%format processGHC = "process'' "

> processGHC :: [Int] -> Int
> processGHC xs = loop' xs 0
>
> loop' :: [Int] -> Int -> Int
> loop'  []      n  = n
> loop'  (x:xs)  n  = loop' xs (n + x * x)

\end{frame}


\begin{frame}
\frametitle{We want simpe/readible programs that are also performant}

Our original version was simple to understand, but not fast.

< process xs = sum0 . map0 sq $ xs

Our accumulator one is fast but not simple.

< processFused' :: [Int] -> Int
< processFused' = loop 0
<   where
<     loop n  []      = n
<     loop n  (x:xs)  = loop (n + x * x) xs



\emph{How can we leverage the compile to write simple code that is fast?}

\end{frame}


\section{A brief introduction to GHC}

\begin{frame}
\frametitle{The GHC Compilation Pipeline converts Haskell into an intermediate language and then bytecode}

When GHC compiles a Haskell program, it converts the code into an
intermediate language called "Core", which is then (eventually) turned into
byte code.

\includegraphics[width=\textwidth]{figs/ghcpipe.pdf}

\end{frame}

\begin{frame}
\frametitle{The GHC Compilation Pipeline converts Haskell into an intermediate language and then bytecode}

When GHC compiles a Haskell program, it converts the code into an
intermediate language called "Core", which is then (eventually) turned into
byte code.

\includegraphics[width=\textwidth]{figs/ghcpipe_core.pdf}

\end{frame}


\begin{frame}
\frametitle{GHC performs several program transformations on Core to optimize the code}

When GHC is given a Core program, it performs several types of
transformations on the program.

\begin{itemize}[<+->]
  \item Inlining functions
  \item Removing redundant lambdas
  \item Simplifying constant expressions (|(x + 8) - 1|)
  \item Combining type casts
  \item \emph{Applying rewrite rules}
  \item \textellipsis
\end{itemize}

\end{frame}

%format map1 = "map_f"

%if False

> {-# NOINLINE map1 #-}
> map1  _  []      = []
> map1  f  (x:xs)  = f x : map1 f xs

%endif

\begin{frame}
\frametitle{Rewrite Rules allow us to say two expressions are equivalent}

Rewrite rules allow us to replace terms in the program with equivalent terms.

< {-# RULES "name" forall x. id x = x #-}

\pause

"Any time we see the term |id x|, replace it with |x|".

\end{frame}

\begin{frame}
\frametitle{Rules have some restrictions}

Rewrite rules have some gotchas.

\begin{itemize}[<+->]

\item Rules doesn't prevent you from doing something silly

< {-# RULES "id5" forall x. id x = 5 #-}

\item The left hand side is only substituted for the right, not the other way around.

%format `notconvert` = "\nRightarrow"

< {-# RULES "id" forall x. id x = x #-}
< x `notconvert` id x

\item You can make the compiler go into an infinite loop.

< {-# RULES "fxy" forall x y. f x y = f y x #-}

\item If multiple rules are possible, GHC will randomly choose one.

\end{itemize}

\end{frame}


\begin{frame}
\frametitle{We can combine maps to traverse a list once}

Let us introduce the following rule about maps.

%if False

> {-# RULES "map/map" forall f g xs. map1 f (map1 g xs) = map1 (f.g) xs #-}

%endif

%format map1 = "\text{map}_{\text{f}} "

< {-# RULES "|map1|/|map1|" forall f g xs. |map1| f (|map1| g xs)
<                                  = |map1| (f.g) xs #-}

%format map1 = "map_f"

\pause

> mapTest0 :: [Int] -> [Int]
> mapTest0 xs = map0 (+1) (map0 (*2) xs)

> mapTest1 :: [Int] -> [Int]
> mapTest1 xs = map1 (+1) (map1 (*2) xs)

\end{frame}


\begin{frame}
\frametitle{Our map fusion performs (a bit) better!}

We can test our functions on a million elements

< mapTest0 xs = map0 (+1) (map0 (*2) xs)
< mapTest0 [1..million]

< mapTest1 xs = map1 (+1) (map1 (*2) xs)
< mapTest1 [1..million]

and find we get a bit better time and space performance.

\begin{center}
\begin{tabular}{rcc}
\toprule
  Function & Time (ms) & Memory (MB) \\
  \midrule
  |mapTest0|      & 26.4 & 256.00 \\
  |mapTest1|      & 17.6  & 184.00\\
  \bottomrule
\end{tabular}
\end{center}

\end{frame}

\begin{frame}
\frametitle{Through rules, GHC performs fusion}

Some of the rules work together to perform \emph{fusion}: to combine terms
in such a way as to pass over a data structure once.

In our |process| function, we create an intermediate list

< process xs = sum0 . map0 sq $ xs

whereas our "fused" form did not make any intermediate structure, and used
an accumulator instead.

< processFused' :: [Int] -> Int
< processFused' = loop 0
<   where
<     loop n  []      = n
<     loop n  (x:xs)  = loop (n + x * x) xs


\end{frame}

\section{List fusion with |foldr|/|build|}

\begin{frame}
\frametitle{|foldr/build| fusion is used to simplify list computations}

GHC accomplishes fusion with two functions: foldr and build.

\pause

%format foldr1 = "foldr"

|foldr1| combines the elements of a list


> foldr1  _  b  []      =  b
> foldr1  f  b  (a:as)  =  foldr1 f (f a b) as

\pause

%format build1 = "build"

while |build1| builds up a list from a generating function.

> build1 :: (forall b. (a -> b -> b) -> b -> b) -> [a]
> build1 g = g (:) []

< build1 l == [1,2,3]
<   where
<     l cons nil = 1 `cons` (2 `cons` (3 `cons` nil))

\end{frame}

\begin{frame}
\frametitle{The |foldr1|/|build1| rule removes intermediate fold/build pairs}

To remove intermediate data structures (those created by |build1|), we eliminate |foldr1|/|build1| pairs with a rule.

%if False

> {-# RULES
> "build1/foldr1" forall f z (g :: forall b. (a -> b -> b) -> b -> b). foldr1 f z (build1 g) = g f z
>   #-}

%endif

< {-# RULES
< "|build1|/|foldr1|" forall f z (g :: forall b. (a -> b -> b) -> b -> b). |foldr1| f z (|build1| g) = g f z
<   #-}


< foldr (+) 0 (build l) == l (+) 0 == 1 + (2 + (3 + 0))
<   where
<     l cons nil = 1 `cons` (2 `cons` (3 `cons` nil))


\end{frame}

\begin{frame}
\frametitle{We need a few extra rules to convert maps into fold/builds}

%format map2 = "map"
%format foldr2 = "foldr"
%format mapFB = "map_{fb}"

%if False

> {-# NOINLINE map2 #-}
> {-# INLINE [0] mapFB #-}

%endif

To convert our definition of maps into a fold/build pair, we need the
following helper function.

> mapFB :: (b -> l -> l) -> (a -> b) -> a -> l -> l
> mapFB cons f = \x ys -> f x `cons` ys

\pause

With that, we have all we to convert map into build/fold.

< {-# RULES "|map2|" forall f xs. |map2| f xs = |build1| (\c n -> |foldr1| (|mapFB| c f) n xs) #-}

\pause

We also provide a way to combine sequential |mapFB| functions.

> {-# RULES "mapFB" forall c f g. mapFB (mapFB c f) g = mapFB c (f . g) #-}


%if False

> {-# RULES "map2"     [~1] forall f xs. map2 f xs = build (\c n -> foldr1 (mapFB c f) n xs) #-}
> {-# "map2List" [1]  forall f.    foldr2 (mapFB (:) f) [] = map2 f #-}

%endif

\end{frame}

\begin{frame}
\frametitle{This is where the example is expanded completely}

Will show how the map sum example is converted into the loop we saw.

Will need

> foldr2 :: (a -> b -> b) -> b -> [a] -> b
> foldr2 f z = go
>   where
>     go []     = z
>     go (y:ys) = y `f` go ys
> {-# INLINE [0] foldr2 #-}

\end{frame}

\section{Stream Fusion}

\section{Applications of Stream Fusion}

\begin{frame}
\frametitle{We can make |process| even faster with |Data.Vector|}

The |Data.Vector| package uses stream fusion and many other rewrite rules
behind the scenes in order to optimize array based computations.

< process xs = sum0 . map0 sq $ xs

\pause

The vector version looks very similar.

< import qualified Data.Vector as V

> processVec n = V.sum $ V.map sq $ V.enumFromTo 1 (n :: Int)

\end{frame}

\begin{frame}
\frametitle{We can make |process| even faster with |Data.Vector|}

But has incredible performance!

\begin{center}
\begin{tabular}{rcc}
\toprule
  Function & Time (ms) & Memory (MB) \\
  \midrule
  |process|       & 220.0 & 265.26 \\
  |process'|      & 5.1   & 80.00\\
 %|processFused|  & 26.8  & 96.65\\
  |processFused'| & 4.7   & 80.00 \\
  process.c       & 2.6   & $8\times10^{-5}$ \\
  processVec      & 0.7   & $16\times10^{-5}$ \\
  \bottomrule
\end{tabular}
\end{center}

\end{frame}

\begin{frame}
\frametitle{What code does |Data.Vector| generate?}

While we wrote this in our program

< processVec n = V.sum $ V.map sq $ V.enumFromTo 1 (n :: Int)

GHC ended up generating the following Core code.

< val_$s$wfoldlM'_loop
< val_$s$wfoldlM'_loop =
<   \ sc sc1 ->
<     case tagToEnum# (<=# sc 100000000#) of _ {
<       False -> sc1;
<       True -> val_$s$wfoldlM'_loop (+# sc 1#) (+# sc1 (*# sc sc))
<     }

\rnote{simplify this core, ignore unboxing}

\end{frame}

\begin{frame}
\frametitle{Repa: A numerical Haskell Library using Fusion}

Repa also uses fusion in order to handle array operations.

\end{frame}

\begin{frame}
\frametitle{Data Parallel Haskell: Nested Data Parallelism made easy}

< processDPH :: [: Int :] -> Int
< processDPH = sumDPH . mapDPH sq $ xs

Does dispatch by MPI.

\end{frame}

%if False


% mapTest0       255,999,920  488  OK
% time                 26.38 ms   (25.07 ms .. 27.95 ms)
% mapTest1       183,999,920  355  OK
% time                 17.62 ms   (17.23 ms .. 18.08 ms)

%endif

\end{document}
