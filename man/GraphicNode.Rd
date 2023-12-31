% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Class_TreeStructureInR6.R
\name{GraphicNode}
\alias{GraphicNode}
\title{The graphic node for compulating the location and visulization.}
\description{
The graphic node for compulating the location and visulization.

The graphic node for compulating the location and visulization.
}
\details{
It add two novel properties, xAxis_or_radius and yAxis_or_angle.
}
\section{Super class}{
\code{\link[PathwayIlluminator:ListTreeNode]{PathwayIlluminator::ListTreeNode}} -> \code{GraphicNode}
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{xAxis_or_radius}}{In circular layout the xAxis become the radius, so as for the spiral shape}

\item{\code{yAxis_or_angle}}{In circular layout the yAxis become the angle, so as for the spiral shape}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-GraphicNode-clone}{\code{GraphicNode$clone()}}
}
}
\if{html}{\out{
<details open><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="PathwayIlluminator" data-topic="ListTreeNode" data-id="addChild"><a href='../../PathwayIlluminator/html/ListTreeNode.html#method-ListTreeNode-addChild'><code>PathwayIlluminator::ListTreeNode$addChild()</code></a></span></li>
<li><span class="pkg-link" data-pkg="PathwayIlluminator" data-topic="ListTreeNode" data-id="getIDName"><a href='../../PathwayIlluminator/html/ListTreeNode.html#method-ListTreeNode-getIDName'><code>PathwayIlluminator::ListTreeNode$getIDName()</code></a></span></li>
<li><span class="pkg-link" data-pkg="PathwayIlluminator" data-topic="ListTreeNode" data-id="initialize"><a href='../../PathwayIlluminator/html/ListTreeNode.html#method-ListTreeNode-initialize'><code>PathwayIlluminator::ListTreeNode$initialize()</code></a></span></li>
<li><span class="pkg-link" data-pkg="PathwayIlluminator" data-topic="ListTreeNode" data-id="isLeaf"><a href='../../PathwayIlluminator/html/ListTreeNode.html#method-ListTreeNode-isLeaf'><code>PathwayIlluminator::ListTreeNode$isLeaf()</code></a></span></li>
<li><span class="pkg-link" data-pkg="PathwayIlluminator" data-topic="ListTreeNode" data-id="print"><a href='../../PathwayIlluminator/html/ListTreeNode.html#method-ListTreeNode-print'><code>PathwayIlluminator::ListTreeNode$print()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-GraphicNode-clone"></a>}}
\if{latex}{\out{\hypertarget{method-GraphicNode-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{GraphicNode$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
