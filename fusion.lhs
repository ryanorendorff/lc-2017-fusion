%if False

> {-# LANGUAGE BangPatterns #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE GADTs #-}
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
%include forall.fmt

\usepackage{appendixnumberbeamer}
\usepackage{booktabs}
\usepackage[numbers,sort&compress]{natbib}

\usepackage{listings}
\lstset{basicstyle=\ttfamily, escapeinside={!!}, mathescape=true}


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

\newcommand{\typeanddef}[3]{\newcommand{#1}{#2 \vspace{-1em} #3}}

\newcommand{\definefunc}[3]{
  \expandafter\newcommand\csname #1type\endcsname{#2}
  \expandafter\newcommand\csname #1def\endcsname{#3}
  \expandafter\newcommand\csname #1\endcsname{
    #2

    \vspace{-2.25em}

    #3

    \vspace{-2.00em}
  }
}

\title{Fusion: Applying Equational Transforms to Simplify Programs}
\subtitle{\verb|github.com/ryanorendorff/lc-2017-fusion|}
\author{Ryan~Orendorff\inst{1}}

\institute{
  \inst{1}%
  Department of Bioengineering\\
  University of California, Berkeley\\
  University of California, San Francisco
}

\date{May 2017}

\begin{document}

\frame{\titlepage}

\begin{frame}
\frametitle{Outline}
\tableofcontents[]
\end{frame}

\AtBeginSection[] {}
\section{Motivation: Simple Programs versus Performance}

\AtBeginSection[]
{
  \begin{frame}
    \frametitle{Table of Contents}
    \tableofcontents[currentsection]
  \end{frame}
}

%format million = "1,000,000"

%format mapunfused = "\Varid{map}"
%format foldrunfused = "\Varid{foldr}"
%format sumunfused = "\Varid{sum}"

\definefunc{process}{

> process :: [Int] -> Int

}{

> process xs = sumunfused . mapunfused sq $ xs

}

%if False

> -- We define this function for testing against what GHC natively does.
> process' :: [Int] -> Int
> process' xs = foldr (+) 0 . map sq $ xs

%endif



%format processmanualfused = "\Varid{process_{hand}} "
\definefunc{processmanualfused}{

> processmanualfused :: [Int] -> Int

}{

> processmanualfused []     = 0
> processmanualfused (x:xs) = x * x + processmanualfused xs

}

\begin{frame}
\frametitle{Common way to process a list: map and fold!}

As an example, say we want to square all the elements in a list and then sum
the result. \cite{Wadler:1990ix,Coutts:2007up,Coutts:2010um,Karpov:2016wn}

\process

Where we have defined the functions as follows.

\pause

> mapunfused  _  []      = []
> mapunfused  f  (x:xs)  = f x : mapunfused f xs
>
> sq x = x*x

%if False

> {-# INLINE sq #-}

%endif

\pause

% Note that this definition is itself far more efficient than then following
% more standard definition of foldr
%
% foldrunfused _ b []     = b
% foldrunfused f b (a:as) = foldrunfused f (f a b) as


> foldrunfused :: (a -> b -> b) -> b -> [a] -> b
> foldrunfused  _  z  []      =  z
> foldrunfused  f  z  (x:xs)  =  f x (foldrunfused f z xs)
>
> sumunfused = foldrunfused (+) 0

\end{frame}

\begin{frame}
\frametitle{How fast is |process|?}

So now that we have our process function, how fast does it run?

\process

Let's try to process a million elements with our |process| and |process'|,
which uses the standard Prelude |foldr| and |map|.

< process   [0..million]; process'  [0..million]

\pause

\vspace{-1.5em}

\begin{center}
\begin{tabular}{rcc}
\toprule
  Function   & Time (ms) & Memory (MB) \\
  \midrule
  |process|  & 41.86     & 265.26      \\
  |process'| & 25.31     & 96.65       \\
  \bottomrule
\end{tabular}
\end{center}

\pause

\emph{How does the Prelude do better with the same functions?}

\end{frame}


\begin{frame}
\frametitle{We can get good performance with manual code}

We can try to get better performance by writing our program as a recursive
function.

\process

\pause

\processmanualfused

\pause

\begin{center}
\begin{tabular}{rcc}
\toprule
  Function              & Time (ms) & Memory (MB) \\
  \midrule
  |process|             & 41.86     & 265.26      \\
  |process'|            & 25.31     & 96.65       \\
  |processmanualfused|  & 26.80     & 96.65       \\
  \bottomrule
\end{tabular}
\end{center}

It seems we have matched GHC's performance!

\end{frame}


%if False

\begin{frame}[fragile]
\frametitle{Let's try to optimize with an accumulator}

What if we try to optimize |process| using an accumulator?

\process

\pause

This means we make a tail-recursive "loop".

> processmanualfused' :: [Int] -> Int
> processmanualfused' = loop 0
>   where
>     loop n  []      = n
>     loop n  (x:xs)  = loop (n + x * x) xs

\pause

\vspace{-1.5em}

This mirrors the version of the program one would write imperatively.

\end{frame}

\begin{frame}
\frametitle{How well does our manually fused version do?}

< process xs = sum0 . mapunfused sq $ xs
< process' xs = Prelude.sum . Prelude.map sq $ xs
<
< processmanualfused' :: [Int] -> Int
< processmanualfused' = loop 0
<   where
<     loop n  []      = n
<     loop n  (x:xs)  = loop (n + x * x) xs

\pause
\vspace{-1.5em}

\begin{center}
\begin{tabular}{rcc}
\toprule
  Function              & Time (ms) & Memory (MB) \\
  \midrule
  |process|             & 41.86     & 265.26 \\
  |process'|            & 25.31     & 96.65\\
  |processmanualfused'| & 4.7       & 96.65 \\
  \bottomrule
\end{tabular}
\end{center}

\end{frame}

%endif

\section{A brief introduction to GHC}

\begin{frame}
\frametitle{The GHC Compilation Pipeline converts Haskell into an intermediate language and then bytecode}

When GHC compiles a Haskell program, it converts the code into an
intermediate language called "Core", which is then (eventually) turned into
byte code. \cite{Team:Gi8C1ZU-}

\includegraphics[width=\textwidth]{figs/ghcpipe.pdf}

\end{frame}

\begin{frame}
\frametitle{The GHC Compilation Pipeline converts Haskell into an intermediate language and then bytecode}

When GHC compiles a Haskell program, it converts the code into an
intermediate language called "Core", which is then (eventually) turned into
byte code. \cite{Team:Gi8C1ZU-}

\includegraphics[width=\textwidth]{figs/ghcpipe_core.pdf}

\end{frame}


%format `trans` = "\Rightarrow "

\begin{frame}
\frametitle{GHC performs several program transformations on Core to optimize the code}

When GHC is given a Core program, it performs several types of
transformations on the program. \cite{Team:aqSC0Vao}

\begin{itemize}[<+->]
  \item Inlining functions
  \item Applying a function to its arguments (|(\x -> x + y) 4 `trans` 4 + y|)
  \item Simplifying constant expressions (|x + 8 - 1 `trans` x + 7|)
  \item Reordering case and let expressions
  \item \emph{Applying rewrite rules}
  \item \textellipsis
\end{itemize}

\end{frame}

\begin{frame}
\frametitle{Rewrite Rules allow us to say two expressions are equivalent}

Rewrite rules allow us to replace terms in the program with equivalent terms. \cite{playbyrules}

\begin{verbatim}
{-# RULES "name" [#] forall x. id x = x #-}
\end{verbatim}

\pause

\begin{itemize}[<+->]
  \item "name" is just for us to read when debugging
  \item \verb|[#]| represents what phase the rule is applied (phases 4-0). This annotation is optional.
  \item The \verb|forall| brings a variable into scope. Sometimes written $\forall$.
  \item After the period are the equivalent statements.
\end{itemize}


\end{frame}

\begin{frame}
\frametitle{Rules have some restrictions}

Rewrite rules have some gotchas. \cite{Team:v0F8esqC}

\begin{itemize}[<+->]

\item Rules doesn't prevent you from doing something silly

\begin{verbatim}
{-# RULES "id5" forall x. id x = 5 #-}
\end{verbatim}

\item The left hand side is only substituted for the right, not the other way around.

\begin{verbatim}
{-# RULES "id" forall x. id x = x #-}
\end{verbatim}
$ x \nRightarrow id\ x$

\item You can make the compiler go into an infinite loop.

\begin{verbatim}
{-# RULES "fxy" forall x y. f x y = f y x #-}
\end{verbatim}

\item If multiple rules are possible, GHC arbitrarily chooses one.

\end{itemize}

\end{frame}

%format mapTestUnfused = "\Varid{mapTest}"
\definefunc{maptestunfused}{

> mapTestUnfused :: [Int] -> [Int]

}{

> mapTestUnfused xs = mapunfused (+1) (mapunfused (*2) xs)

}

%format mapTestFused = "\Varid{mapTest_{fuse}}"
%format mapfuse = "\Varid{map_{fuse}}"
\definefunc{maptestfused}{

> mapTestFused :: [Int] -> [Int]

}{

> mapTestFused xs = mapfuse (+1) (mapfuse (*2) xs)

}

\begin{frame}[fragile]
\frametitle{We can combine maps to traverse a list once}

Let us introduce the following rule about maps. \cite{Karpov:2016wn}


\begin{lstlisting}
{-# RULES "map/map" forall f g xs.
  !map$_{\mathrm{fuse}}$! f (!map$_{\mathrm{fuse}}$! g xs) =  !map$_{\mathrm{fuse}}$! (f.g) xs #-}
\end{lstlisting}


\pause

\maptestunfusedtype

\vspace{-2em}

\maptestunfuseddef


\maptestfusedtype

\vspace{-2em}

\maptestfuseddef

\end{frame}


\begin{frame}
\frametitle{Our map fusion performs (a bit) better!}

We can test our functions on a million elements

\maptestunfuseddef

\vspace{-2em}

\maptestfuseddef

and find we get a bit better time and space performance.

\begin{center}
\begin{tabular}{rcc}
\toprule
  Function & Time (ms) & Memory (MB) \\
  \midrule
  |mapTestUnfused|      & 26.4 & 256.00 \\
  |mapTestFused|      & 17.6  & 184.00\\
  \bottomrule
\end{tabular}
\end{center}

\end{frame}

\begin{frame}
\frametitle{Through rules, GHC performs fusion}

Rules allow us to perform \emph{fusion}, where we remove intermediate data structures from the computation.

In our |process| function, we create an intermediate list

\process

whereas our "fused" form did not make any intermediate structure, and used
an accumulator instead.

\processmanualfused

\end{frame}


%format sumfuse = "\Varid{sum}"
%format processfuse = "\Varid{process}"
%format processFuse = "\Varid{process_{fuse}}"

%if False

> mapfuse :: (a -> b) -> [a] -> [b]
> mapfuse _ []     = []
> mapfuse f (x:xs) = f x : mapfuse f xs

> sumfuse :: [Int] -> Int
> sumfuse =  foldrfuse (+) 0

> processFuse :: [Int] -> Int
> processFuse = sumfuse . mapfuse sq

> {-# NOINLINE [0] mapfuse #-}
> {-# INLINE sumfuse #-}

> {-# RULES
> "foldfuse/buildfuse"    forall k z (g::forall b. (a->b->b) -> b -> b) .
>                 foldrfuse k z (buildfuse g) = g k z
>  #-}
>
> {-# RULES
> "mapfuse/mapfuse"       forall f g xs.   mapfuse f (mapfuse g xs)                = mapfuse (f.g) xs
> "mapfuse"       [~1] forall f xs.   mapfuse f xs                = buildfuse (\c n -> foldrfuse (mapFBfuse c f) n xs)
> "mapfuseList"   [1]  forall f.      foldrfuse (mapFBfuse (:) f) []  = mapfuse f
> "mapFBfuse"     forall c f g.       mapFBfuse (mapFBfuse c f) g     = mapFBfuse c (f.g)
>   #-}
>

%endif


\section{List fusion with |foldr|/|build|}

\begin{frame}
\frametitle{|foldr/build| fusion is used to simplify list computations}

GHC accomplishes fusion with two functions: foldr and build. \cite{Team:8RXCROes,Coutts:2010um}

\pause

|foldr| combines the elements of a list

%format foldrfuse = "\Varid{foldr}"

> foldrfuse :: (a -> b -> b) -> b -> [a] -> b
> foldrfuse f z [] = z
> foldrfuse f z (x:xs) = f x (foldrfuse f z xs)

%if False

> {-# INLINE [0] foldrfuse #-}

%endif

\pause

while |build| builds up a list from a generating function.

%format buildfuse = "\Varid{build}"

> buildfuse   :: (forall b. (a -> b -> b) -> b -> b) -> [a]
> buildfuse g = g (:) []

%if False

> {-# INLINE [1] buildfuse #-}

%endif

\pause

< build1 l == [1,2,3]
<   where
<     l cons nil = 1 `cons` (2 `cons` (3 `cons` nil))

\end{frame}

\begin{frame}[fragile]
\frametitle{The |foldr|/|build| rule removes intermediate fold/build pairs}

To remove intermediate data structures (those created by |build|), we eliminate |foldr|/|build| pairs with a rule.


\begin{lstlisting}
{-# RULES
"foldr/build"
!$\forall$! f z (g :: !$\forall$! b. (a -> b -> b) -> b -> b).
foldr f z (build g) = g f z #-}
\end{lstlisting}


< foldr (+) 0 (build l) == l (+) 0 == 1 + (2 + (3 + 0))
<   where
<     l cons nil = 1 `cons` (2 `cons` (3 `cons` nil))


\end{frame}

\begin{frame}[fragile]
\frametitle{We need a few extra rules to convert maps into fold/builds}

To convert our definition of maps into a fold/build pair, we need the
following helper function. \cite{Team:8RXCROes,Team:v0F8esqC}

%format mapFBfuse = "\Varid{mapFB}"

> mapFBfuse ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
> mapFBfuse c f = \x ys -> c (f x) ys

%if False

> {-# INLINE [0] mapFBfuse #-}

%endif

\pause

As an example, lets apply the list cons |c = (:)| and |f = sq|

< \x ys -> sq x : ys

\end{frame}

\begin{frame}[fragile]
\frametitle{We need a few extra rules to convert maps into fold/builds}

With that, we have all we need to convert map into build/fold.

\begin{lstlisting}
{-# RULES "map" [~1] !$\forall$! f xs. map f xs =
  build (\c n -> foldr (mapFB c f) n xs) #-}
\end{lstlisting}

\pause

We also provide a way to cancel failed fusion by converting back to a map.

\begin{lstlisting}
{-# RULES "mapList" [1] !$\forall$! f.
  foldr (mapFB (:) f) []  = map f #-}
\end{lstlisting}

\pause

\begin{spec}

  build (\c n -> foldr mapFB c f) n xs)

== {- inline def of build -}

  (\c n -> foldr mapFB c f) n xs) (:) []

== {- remove lambda -}

  foldr (mapFB (:) f) [] xs

\end{spec}

\end{frame}


\begin{frame}
\frametitle{Manual rewrite rule application}

Let's try applying the rewrite rules manually.

\def\commentbegin{\quad\{\ }
\def\commentend{\}}
\begin{spec}
  sum (map sq xs)
\end{spec}
\vspace{-3em}

\pause

\begin{spec}
== {- expand |map f xs| -}
\end{spec}
\vspace{-3em}

\pause

\begin{spec}
  sum (build (\c n -> foldrfuse (mapFBfuse c sq) n xs))
\end{spec}
\vspace{-3em}

\pause

\begin{spec}
== {- expand sum -}
\end{spec}
\vspace{-3em}

\pause

\begin{spec}
  foldrfuse (+) 0 (build (\c n -> foldrfuse (mapFBfuse c sq) n xs))
\end{spec}
\vspace{-3em}

\pause

\begin{spec}
== {- apply |foldr/build|: |foldr f z (build g) = g f z| -}
\end{spec}
\vspace{-3em}

\pause

\begin{spec}
  \c n -> foldrfuse (mapFBfuse c sq) n xs) (+) 0
\end{spec}
\vspace{-3em}

\pause

\begin{spec}
== {- apply lambda -}
\end{spec}
\vspace{-3em}

\pause

\begin{spec}
  foldrfuse (\x ys -> sq x + ys) 0 xs
\end{spec}

\end{frame}


\begin{frame}
\frametitle{Applying foldr: the empty case}

We now look at empty case

\begin{spec}
  foldrfuse (\x ys -> sq x + ys) 0 []
\end{spec}
\vspace{-2em}

\pause

\begin{spec}
== {- expand |foldr| case: |foldr f z []| = z -}
\end{spec}
\vspace{-2em}

\pause

\begin{spec}
0
\end{spec}

\end{frame}

\begin{frame}
\frametitle{Applying foldr: the list case}

Now let's do the |(x:xs)| case.

\begin{spec}
  process (x:xs) = foldrfuse (\x ys -> sq x + ys) 0 (x:xs)
\end{spec}
\vspace{-2em}

\pause

\begin{spec}
== {- expand |foldr| case: |foldr f z (x:xs) = f x (foldr f z xs)| -}
\end{spec}
\vspace{-2em}

\pause

\begin{spec}
(\x ys -> sq x + ys) x (foldr (\x ys -> sq x + ys) z xs)
\end{spec}
\vspace{-2em}

\pause

\begin{spec}
== {- use def of |processFuse|: |foldrfuse f 0 xs = processFuse xs|-}
\end{spec}
\vspace{-2em}

\pause

\begin{spec}
(\x ys -> sq x + ys) x (processFuse xs)
\end{spec}
\vspace{-2em}

\pause

\begin{spec}
== {- apply lambda -}
\end{spec}
\vspace{-2em}

\pause

\begin{spec}
sq x + process xs
\end{spec}
\vspace{-2em}

\pause

\begin{spec}
== {- inline |sq| -}
\end{spec}
\vspace{-2em}

\begin{spec}
x*x + processFuse xs
\end{spec}

\end{frame}

\begin{frame}
\frametitle{Bringing both cases back together}

If we now combine our two cases, we have the following

< processFuse []      =  0
< processFuse (x:xs)  =  x * x + processFuse xs

This is the same as what we had originally written manually!

\processmanualfuseddef

\end{frame}

\begin{frame}
\frametitle{We achieved list fusion using |foldr/build| with rewrite rules}

We managed to fuse |process| using our rewrite rules. We can look at the
output of the compiler and it confirms what we expected.

< processFuse []      =  0
< processFuse (x:xs)  =  x * x + processFuse xs

\pause

As expected, we get the same performance after performing the fusion rules.

\begin{center}
\begin{tabular}{rcc}
\toprule
  Function             & Time (ms) & Memory (MB)      \\
  \midrule
  |process|            & 41.86     & 265.26           \\
  |process'|           & 26.60     & 96.65            \\
 %|processmanualfused| & 26.80     & 96.65            \\
  |processmanualfused| & 28.80     & 96.65            \\
  |processFuse|        & 27.08     & 96.65            \\
  %process.c           & 2.6       & $8\times10^{-5}$ \\
  \bottomrule
\end{tabular}
\end{center}

\end{frame}


\begin{frame}
\frametitle{There are many types of fusion concepts out there}

While |foldr/build| works well, it can have problems fusing |zip| and
|foldl|.

There are a few other systems out there. \cite{Coutts:2007up,Coutts:2010um}

\begin{itemize}[<+->]
  \item |unbuild/unfoldr|, where |unfoldr| builds a list and |unbuild| consumes a list. It can have problems fusing |filter|.
  \item stream fusion, which works by defining a |Stream| data type that acts like an iterator.
\end{itemize}

\end{frame}

\section{Stream Fusion}

\begin{frame}
\frametitle{Introduction to Stream}

The Stream fusion system attempts to do something similar, by defining a
list as an iterator. \cite{Coutts:2010um,Coutts:2007up}

> data Stream a where
>   Stream :: (s -> Step a s) -> s -> Stream a

\pause

where |Step a s| informs us how to keep processing the stream.

> data Step a s  =  Done
>                |  Skip     s
>                |  Yield a  s
>

\end{frame}


%if False

> {-# INLINE [1] stream #-}



> {-# INLINE [1] unstream #-}



> {-# INLINE foldls #-}

> foldls :: (b -> a -> b) -> b -> Stream a -> b
> foldls f a (Stream next s0) = go a s0
>   where
>     go a s = case next s of
>               Done -> a
>               Skip s' -> go a s'
>               Yield x s' -> go (f a x) s'

> {-# INLINE foldll #-}

> foldll :: (b -> a -> b) -> b -> [a] -> b
> foldll f a = foldls f a . stream

> {-# RULES "stream/unstream" forall (s :: Stream a). stream (unstream s) = s #-}

%endif


\begin{frame}
\frametitle{Streams have little helpers to make lists: stream}

To work on standard lists, we introduce the following two functions to
convert between lists and streams.

> stream :: [a] -> Stream a
> stream xs = Stream uncons xs
>   where
>     uncons  []      =  Done
>     uncons  (y:ys)  =  Yield y ys

\end{frame}

\begin{frame}
\frametitle{Streams have little helpers to make lists: unstream}

To work on standard lists, we introduce the following two functions to
convert between lists and streams.

> unstream :: Stream a -> [a]
> unstream (Stream next s0) = unfold next s0
>   where
>     unfold next s = case next s of
>                       Done        -> []
>                       Skip    s'  ->      unfold next s'
>                       Yield x s'  -> x :  unfold next s'


%if False

\pause

Note that these functions are inverses.

%format id_stream
%format list = "[a]"
%format id_list

< stream . unstream == id_stream
< unstream . stream == id_list

%endif

\end{frame}

\begin{frame}
\frametitle{Let's define |map| for |Streams|}

We can define some standard list processing functions on |Streams|. Let's
try |map|.

%if False

> {-# INLINE maps #-}
> {-# INLINE mapl #-}

%endif

%format maps = "\Varid{map_{s}}"

> maps :: (a -> b) -> Stream a -> Stream b
> maps f (Stream next0 s0) = Stream next s0
>   where
>     next s = case next0 s of
>               Done        -> Done
>               Skip    s'  -> Skip         s'
>               Yield x s'  -> Yield (f x)  s'

\pause

%format mapl = "\Varid{map_{[a]}}"

> mapl :: (a -> b) -> [a] -> [b]
> mapl f = unstream . maps f . stream

\end{frame}

\begin{frame}[fragile]
\frametitle{Fusion on Streams}

Fusion on streams only has one rewrite rule, and it is pretty simple.

\begin{lstlisting}
{-# RULES "stream" !$\forall$! (s :: Stream a).
    stream (unstream s) = s #-}
\end{lstlisting}

\pause

> mapTestStream :: [Int] -> [Int]
> mapTestStream xs = mapl (+1) . mapl (*2) $ xs

\pause

\begin{spec}

  mapl (+1) . mapl (*2)

== {- expand mapl -}

  unstream . maps (+1) . stream . unstream . maps (*2) . stream

== {- apply "stream/unstream" -}

  unstream . maps (+1) . maps (*2) . stream

\end{spec}

\end{frame}

\begin{frame}
\frametitle{Map fused by Stream Fusion}

Our map example

< mapTestStream :: [Int] -> [Int]
< mapTestStream xs = mapl (+1) . mapl (*2) $ xs

gets fused into this result.

> mapTestStreamCompiled :: [Int] -> [Int]
> mapTestStreamCompiled [] = []
> mapTestStreamCompiled (x:xs)  =
>   1 + (x*2) : mapTestStreamCompiled xs

\end{frame}


\section{Applications of Fusion}

\begin{frame}
\frametitle{We can make |process| even faster with |Data.Vector|}

The |Data.Vector| package uses stream fusion and many other rewrite rules
behind the scenes in order to optimize array based computations.

< process xs = sum0 . mapunfused sq $ xs

\pause

The vector version looks very similar.

< import qualified Data.Vector as V

> processVec n = V.sum $ V.map sq $ V.enumFromTo 1 (n :: Int)

\end{frame}

\begin{frame}
\frametitle{We can make |process| even faster with |Data.Vector|}

< processVec n = V.sum $ V.map sq $ V.enumFromTo 1 (n :: Int)

But has awesome performance!

\begin{center}
\begin{tabular}{rcc}
\toprule
  Function & Time (ms) & Memory (MB) \\
  \midrule
  |process|      & 41.86 & 265.26 \\
  |processFuse| & 25.31 & 96.65\\
  |processVec|   & 0.7   & $16\times10^{-5}$ \\
  \bottomrule
\end{tabular}
\end{center}



\end{frame}

\begin{frame}
\frametitle{What code does |Data.Vector| generate?}

The |processVec| function is pretty simple in Haskell itself.

< processVec n = V.sum $ V.map sq $ V.enumFromTo 1 (n :: Int)

When compiling, GHC fires \emph{202 rules!}

\pause

Specifically, this appears when using the debug flag
\verb|-ddump-rule-firings|. \cite{vector_fusion:2025}

\begin{verbatim}
...
Rule fired: stream/unstream [Vector]
Rule fired: stream/unstream [Vector]
...
\end{verbatim}

\pause

\end{frame}

\begin{frame}
\frametitle{What code does |Data.Vector| generate?}

The |processVec| function is pretty simple in Haskell itself.

< processVec n = V.sum $ V.map sq $ V.enumFromTo 1 (n :: Int)

And the final code generated is the following.

> processVecGHC n = loop 1 0
>   where
>     loop count acc = case count <= n of
>                        False -> acc
>                        True -> loop (count + 1) (acc + (count * count))

\end{frame}

\begin{frame}
\frametitle{Other use cases for fusion}

Besides vector, fusion is used in a few other places.

\begin{itemize}[<+->]
  \item Repa, a parallel list processing library \cite{Lippmeier:2014fx}
  \item Vector instructions by SIMD \cite{Mainland:2013js}
  \item Pipes, a stream processing library \cite{Gonzalez:2014um}
\end{itemize}

\end{frame}

\begin{frame}
\frametitle{Wrap up}

What did we talk about today?

\begin{itemize}
  \item Goal: simple code that performed as well as a optimized version.
  \item A brief introduction to compilation in GHC and rewrite rules.
  \item |foldr/build| fusion.
  \item Showed a second type of fusion: stream fusion.
  \item Went through some libraries using fusion.
\end{itemize}

\end{frame}

\begin{frame}[allowframebreaks]
        \frametitle{References}
        \bibliographystyle{IEEEtranN}
        \bibliography{fusion}
\end{frame}

\end{document}
