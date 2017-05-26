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
\usepackage{minted}
\usepackage{natbib}

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
the result.

\process

Where we have defined the functions as follows.


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
which uses the standard Prelude |sum| and |map|.

< process   [0..million]; process'  [0..million]

\pause

\vspace{-1.5em}

\begin{center}
\begin{tabular}{rcc}
\toprule
  Function & Time (ms) & Memory (MB) \\
  \midrule
  |process|       & 41.86 & 265.26 \\
  |process'|      & 25.31   & 96.65\\
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
\begin{tabular}{rc}
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

\begin{frame}
\frametitle{GHC generated the simplified version automatically}

Our manual version |processmanualfused|.

\processmanualfused

and when we compile the Prelude defined |process'|, GHC produces

> processGHC :: [Int] -> Int
> processGHC  []      = 0
> processGHC  (x:xs)  = x*x + (processGHC xs)

\pause
\emph{How can we leverage the compiler to write simple code that is fast?}

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
  \item Playing with lambda expressions
  \item Simplifying constant expressions (|(x + 8) - 1|)
  \item Reordering case and let expressions
  \item \emph{Applying rewrite rules}
  \item \textellipsis
\end{itemize}

\end{frame}

\begin{frame}
\frametitle{Rewrite Rules allow us to say two expressions are equivalent}

Rewrite rules allow us to replace terms in the program with equivalent terms.

\begin{verbatim}
{-# RULES "name" [#] forall x. id x = x #-}
\end{verbatim}

\pause

\begin{itemize}[<+->]
  \item "name" is just for us to read when debugging
  \item \verb|[#]| represents what phase the rule is applied (phases 4-0)
  \item The \verb|forall| brings a variable into scope
  \item After the period is the what we are saying are equivalent statements.
\end{itemize}


\end{frame}

\begin{frame}
\frametitle{Rules have some restrictions}

Rewrite rules have some gotchas.

\begin{itemize}[<+->]

\item Rules doesn't prevent you from doing something silly

\begin{verbatim}
{-# RULES "id5" forall x. id x = 5 #-}
\end{verbatim}

\item The left hand side is only substituted for the right, not the other way around.

\begin{verbatim}
{-# RULES "id" forall x. id x = x #-}
\end{verbatim}
$ x \nRightarrow id x$

\item You can make the compiler go into an infinite loop.

\begin{verbatim}
{-# RULES "fxy" forall x y. f x y = f y x #-}
\end{verbatim}

\item If multiple rules are possible, GHC will randomly choose one.

\end{itemize}

\end{frame}

%format mapTestUnfused = "\Varid{mapTest}"
\definefunc{maptestunfused}{

> mapTestUnfused :: [Int] -> [Int]

}{

> mapTestUnfused xs = mapunfused (+1) (mapunfused (*2) xs)

}

%format mapTestFused = "\Varid{mapTest_{fuse}}"
\definefunc{maptestfused}{

> mapTestFused :: [Int] -> [Int]

}{

> mapTestFused xs = mapfuse (+1) (mapfuse (*2) xs)

}

\begin{frame}[fragile]
\frametitle{We can combine maps to traverse a list once}

Let us introduce the following rule about maps.


\begin{lstlisting}
{-# RULES "map/map" forall f g xs.
  !map$_{\mathrm{fuse}}$! f (!map$_{\mathrm{fuse}}$! g xs) =  !map$_{\mathrm{fuse}}$! (f.g) xs #-}
\end{lstlisting}


\pause

\maptestunfused
\maptestfused

\end{frame}


\begin{frame}
\frametitle{Our map fusion performs (a bit) better!}

We can test our functions on a million elements

\maptestunfuseddef
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

Some of the rules work together to perform \emph{fusion}: to combine terms
in such a way as to pass over a data structure once.

In our |process| function, we create an intermediate list

\process

whereas our "fused" form did not make any intermediate structure, and used
an accumulator instead.

\processmanualfused

\end{frame}


%format mapfuse = "\Varid{map}"
%format sumfuse = "\Varid{sum}"
%format processfuse = "\Varid{process}"

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

GHC accomplishes fusion with two functions: foldr and build.

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

> buildfuse   :: forall a. (forall b. (a -> b -> b) -> b -> b) -> [a]
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
following helper function.

%format mapFBfuse = "\Varid{mapFB}"

> mapFBfuse ::  (elt -> lst -> lst) -> (a -> elt) -> a -> lst -> lst
> mapFBfuse c f = \x ys -> c (f x) ys

%if False

> {-# INLINE [0] mapFBfuse #-}

%endif

\pause

With that, we have all we need to convert map into build/fold.

\begin{lstlisting}
{-# RULES "map" !$\forall$! f xs. map f xs =
  build (\c n -> foldr mapFB c f) n xs) #-}
\end{lstlisting}

\pause

%if False

We also provide a way to combine sequential |mapFB| functions.


\begin{lstlisting}
{-# RULES "mapFB" !$\forall$! c f g. mapFB (mapFB c f) g =
                          mapFB c (f . g) #-}
\end{lstlisting}

%endif

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
  \c n -> foldfuse (mapFBfuse c sq) n xs) (+) 0
\end{spec}
\vspace{-3em}

\pause

\begin{spec}
== {- apply lambda -}
\end{spec}
\vspace{-3em}

\pause

\begin{spec}
  foldfuse (\x ys -> sq x + ys) 0 xs
\end{spec}

\end{frame}


\begin{frame}
\frametitle{Applying foldr: the empty case}

We now look at empty case

\begin{spec}
  foldfuse (\x ys -> sq x + ys) 0 []
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
  process (x:xs) = foldfuse (\x ys -> sq x + ys) 0 (x:xs)
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
== {- use definition of |processFuse|: |foldr f 0 xs = processFuse xs|-}
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

\processmanualfuseddef

This is the same as what we had originally written manually!

\end{frame}

\begin{frame}
\frametitle{We achieved list fusion using |foldr/build| with rewrite rules}

We managed to fuse |process| using our rewrite rules. We can look at the
output of the compiler and it confirms what we expected.

\processmanualfuseddef

\pause

\begin{center}
\begin{tabular}{rcc}
\toprule
  Function & Time (ms) & Memory (MB) \\
  \midrule
  |process|       & 41.86 & 265.26 \\
  |process'|      & 25.31   & 96.65\\
 %|processmanualfused|  & 26.80  & 96.65\\
  |processmanualfused| & 25.31   & 96.65 \\
  |processFuse| & 25.31   & 96.65 \\
  %process.c       & 2.6   & $8\times10^{-5}$ \\
  \bottomrule
\end{tabular}
\end{center}

\end{frame}

\section{Stream Fusion}

\begin{frame}
\frametitle{Introduction to Stream}

The Stream fusion system attempts to do something similar, by defining a
list as a state machine.

> data Stream a where
>   Stream :: (s -> Step a s) -> s -> Stream a

\pause

> data Step a s  =  Done
>                |  Skip     s
>                |  Yield a  s
>

\end{frame}


%if False

> {-# INLINE [1] stream #-}

> stream :: [a] -> Stream a
> stream xs = Stream uncons xs
>   where
>     uncons []      =  Done
>     uncons (x:xs)  =  Yield x xs


> {-# INLINE [1] unstream #-}


> unstream :: Stream a -> [a]
> unstream (Stream next s0) = unfold next s0
>   where
>     unfold next s = case next s of
>                       Done -> []
>                       Skip s' -> unfold next s'
>                       Yield x s' -> x : unfold next s'

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
\frametitle{Streams have little helpers to make lists}

To work on standard lists, we introduce the following two functions to
convert between lists and streams.

< steam     ::  [a] -> Stream a
< unstream  ::  Stream a -> [a]

Note that these functions are inverses.

%format id_stream
%format list = "[a]"
%format id_list

< stream . unstream == id_stream
< unstream . stream == id_list

\end{frame}

\begin{frame}
\frametitle{Maps on Streams!}

%if False

> {-# INLINE maps #-}
> {-# INLINE mapl #-}

%endif


> maps :: (a -> b) -> Stream a -> Stream b
> maps f (Stream next0 s0) = Stream next s0
>   where
>     next s = case next0 s of
>               Done -> Done
>               Skip s' -> Skip s'
>               Yield x s' -> Yield (f x) s'

\pause

> mapl :: (a -> b) -> [a] -> [b]
> mapl f = unstream . maps f . stream

\end{frame}

\begin{frame}[fragile]
\frametitle{Stream Fusion!}

Fusion on streams only has one rewrite rule, and it is pretty simple.

\begin{lstlisting}
{-# RULES "stream" !$\forall$! (s :: Stream a).
    stream (unstream s) = s #-}
\end{lstlisting}

\pause

> mapTestStream :: [Int] -> [Int]
> mapTestStream xs = mapl (+1) (mapl (*2) xs)

\pause

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

But has incredible performance!

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

While we wrote this in our program

< processVec n = V.sum $ V.map sq $ V.enumFromTo 1 (n :: Int)

GHC then generates the following code (simplified back to Haskell).

\pause

> processVecGHC n = loop 1 0
>   where
>     loop count acc = case count <= n of
>                        False -> acc
>                        True -> loop (count + 1) (acc + (count * count))

\end{frame}

\begin{frame}
\frametitle{Repa: A numerical Haskell Library using Fusion}

Repa also uses fusion in order to handle parallel array operations.

< import qualified Data.Array.Repa as R

> processRepa n = R.foldP (+) 0 . R.map sq $ array
>   where
>     array = R.fromListUnboxed (R.Z R.:. (n :: Int)) [1..n]

\end{frame}

\end{document}
