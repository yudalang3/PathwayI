############################################### statment
# Here we define all the classes, we need.
# Why R6:
# The biggest reason is the R default OOP systems(S3/S4) is not support the reference semantics.
# When we use such grammars, it always trigger the copy-on-modify mechanism.
#
# Another reason is that We do not only implement it in R, but also other languages.
# So it is a better choice.
#
# We think the separate of properties and methods in S3/S4 OOP is so waste of namespace.
# A class should consist of the properties and methods.
############################################### statment

# Define the toppest and the most generic tree node -------------------------------------------------
# Construct the List Tree Node, we not use the Ring Tree Node as suggested by the Joseph Felsenstein in the <Phylogenetics> book
ListTreeNode <- R6Class(
  classname = "ListTreeNode",
  public = list(
    # an static id, used to construct the CGBID: see https://doi.org/10.1093/bib/bbab583
    id = 1,
    # the branch length of the node
    branchLength = 1,
    # label, usually for the leaf, a.k.a the leaf name
    label = NA_character_,
    # the number of leaves that are descendants of the current node
    members = 1,
    # the child nodes
    # We use the
    children = list(),
    # Construct function
    initialize = function(id = 1) {
      self$id <- id
    },
    # add a Child for the node
    addChild = function(child) {
      len <- length(self$children)

      ret <- list()
      if (len > 0) {
        for (i in seq_len(len)) {
          ret[[i]] <- self$children[[i]]
        }
      }
      ret[[len + 1]] <- child


      self$children <- ret

    },

    # return is this a leaf.
    isLeaf = function() {
      return(length(self$children) == 0)
    },

    print = function() {
      cat("ID: ",
          self$id,
          "\tnum of children are: ",
          length(self$children),
          "\tlabel: ", self$label,
          "\tmembers: ", self$members,
          "\n")
    },

    # get the CGB ID that can uniquely represent the node.
    getIDName =  function() {
      if (self$isLeaf()) {
        return(paste("ID ", self$id))
      } else {
        return(sprintf("id is %d.%d", self$id[1], self$id[2]))
      }
    }

  )
)


# Define the Graphic node inherit from the generic node --------------------------------------------
GraphicNode <- R6Class(
  classname = "GraphicNode",
  inherit = ListTreeNode,
  # Why this property is could xAxis_or_radius,
  # Because In circular layout the xAxis become the radius, so as for the spiral shape.
  public = list(xAxis_or_radius = 0,
                yAxis_or_angle = 0)

)

# Define the Data structure for the GraphicTree --------------------------------------------
# It most has a rootNode
# the longestDepth and numOfLeaves is employed for the layout computation
GraphicTree <- R6Class(classname = "GraphicTree", public = list(
  # following are general properties
  rootNode = NA,
  longestDepth = 0,
  numOfLeaves = 0,
  isPropertiesAssigned = F,
  # following properties are for circular layout
  startDegree = 0,
  extendDegree = 360,
  innerRadius = 0.1,
  outterRadius = 0.5,
  direction = 0,
  xCenter = 0.5,
  yCenter = 0.5,

  initialize = function(rootNode) {
    self$rootNode <- rootNode;
  },
  assignAttributes = function(){
    self$longestDepth = getLongestDepth(self$rootNode);

    if (self$rootNode$members <= 1) {
      assignTheCGBID(self$rootNode)
    }

    self$numOfLeaves = self$rootNode$members
  }

))

# Following are some convenient methos for the tree and node-------------------------------------

#' Display the tree by the simplest approach, that is , a table-like statement.
#'
#'@description
#' <pre>
#' For example, you have a tree like:
#'       |-------1
#'       |
#'    ---1.2
#'       |
#'       |---------2
#'
#' The format is :
#' parent node    children number    child id
#'
#' So, you get:
#' Current 1.2    2    1,2
#' Current 1  is leaf.
#' Current 2  is leaf.
#' </pre>
#'
#' @param node: a node, may not be the rootNode.
#' @param fun: function(node), user defined fucntion for customized display.
#'
#' @return nothing to return
#' @export
#'
#' @examples
#' displayTheTree(rootNode)
#' displayTheTree(rootNode, function(x){
#'     cat("Hi ", x$xAxis_or_radius, "\n")
#' })
displayTheTree <- function(node, fun = NULL) {
  if (!is.null(fun)) {
    fun(node)
  }
  cat("Current ", node$getIDName())
  if (node$isLeaf()) {
    cat("\t is leaf.\n")
  } else {
    cat("\tChildren: ", length(node$children))
    for (child in node$children) {
      cat("\t", child$getIDName())
    }
    cat("\n")
    for (child in node$children) {
      displayTheTree(child, fun)
    }
  }

  invisible();
}

# A traversal template
# Not export, for developments only
traversal <- function(node) {
  node$print()

  if (node$isLeaf()) {

  } else {
    for (child in node$children) {
      traversal(child)
    }
  }

  invisible()
}

#' A template to traversal all nodes.
#'
#' @param node: any tree node.
#' @param fun : the function to input, the input parameter is only the node.
#'
#' @return nothing to return
#' @export
#'
#' @examples
#' printSomeThing <- function(node){
#'    cat("Hi I am print ", node$id, "\n")
#' }
#' preOrder_traversal(rootNode, printSomeThing)
preOrder_traversal <- function(node, fun) {
  fun(node);

  if (node$isLeaf()) {

  } else {
    for (child in node$children) {
      traversal(child)
    }
  }

  invisible()
}

#' Create a quick simulated tree.
#' @description
#' The tree is balanced and full children size. <br>
#' What is this? Try yourself.
#'
#' @param depth the depth of the tree, since the tree is balanced, so the tree look same. @seealso [getLongestDepth()]
#' @param numOfChildren n-fork tree
#'
#' @return the tree root node.
#' @export
#'
#' @examples
#'
#' process_createNodes(depth = 2, numOfChildren = 3)
process_createNodes <- function(depth = 1, numOfChildren = 2) {
  root <- GraphicNode$new()


  storage_env <- GraphicTree$new(root)
  storage_env$numOfLeaves <- 0


  recursiveCreateNodes <- function(node, depth = 1) {
    if (depth == 0) {
      storage_env$numOfLeaves <- storage_env$numOfLeaves + 1

      node$id <- storage_env$numOfLeaves
      return()
    } else {
      newDepth <- depth - 1

      for (i in 1:numOfChildren) {
        newNode1 <- GraphicNode$new()
        node$addChild(newNode1)
        recursiveCreateNodes(newNode1, newDepth)
      }

    }
    invisible(node)
  }

  recursiveCreateNodes(root, depth = depth)
  cat("The count is: ", storage_env$numOfLeaves , "\n")

  assignTheCGBID(root)

  root$branchLength <- 0;
  storage_env$rootNode <- root
  storage_env$longestDepth <- getLongestDepth(root)
  return(storage_env)
}

#' The the longest depth of a tree.
#'
#' @description
#'
#' For a tree, it has lots of lineages, from root to the leaf.
#' This function will find the longest depth from root to a specific leaf.
#'
#' @param node : root node
#'
#' @return : the value of the longest depth.
#' @export
#'
#' @examples
#' getLongestDepth(rootNode)
getLongestDepth <- function(node) {
  currentlongestDepth <- 0

  if (node$isLeaf()) {
    currentlongestDepth <- node$branchLength
  } else {
    tempMax <- -1

    for (child in node$children) {
      dep <- getLongestDepth(child)
      if (dep > tempMax) {
        tempMax <- dep
      }
    }
    currentlongestDepth <- tempMax + node$branchLength

  }

  invisible(currentlongestDepth)
}


#' Assign the CGB ID for all nodes.
#'
#' @description
#' When the tree is generated, we assign the id according to the CGBID algorithm.
#'
#' see https://doi.org/10.1093/bib/bbab583
#'
#' @param node: root node
#'
#' @return the minimum value ID among the children.
#' @export
#'
#' @examples
#' assignTheCGBID(rootNode)
assignTheCGBID <- function(node) {
  if (node$isLeaf()) {
    node$members <- 1;
  } else {
    # 先声明两个最大值和最小值
    firstMinValue <- 999998
    secondMinValue <- 999999
    sum <- 0;
    for (child in node$children) {
      #注意需要添加上min字符
      value <- assignTheCGBID(child)
      if (value < firstMinValue) {
        secondMinValue <- firstMinValue
        firstMinValue <- value
      } else if (value < secondMinValue) {
        secondMinValue <- value
      }
      sum <- child$members + sum;
    }
    node$id <- c(firstMinValue, secondMinValue)
    node$members <- sum;
  }
  invisible(min(node$id))
}


# Tree conversion -------------------------------------------------------

### following Codes are for debug
# ?hclust()
#
# hc <- hclust(dist(USArrests), "ave")
# plot(hc)
# plot(hc, hang = -1)
#
# dendrogram <- as.dendrogram(hc)
# class(dendrogram)
# str(dendrogram)
# attributes(dendrogram)
# attributes(dendrogram[[1]])
#
# leaf <- dendrogram[[c(1,1,1)]]
# attributes()
#
# attributes(dendrogram[[2]])


# preOrderIteration <- function(node) {
#   cat("Node: ",
#       attr(x = node, which = 'members') ,
#       "\t" ,
#       attr(x = node, which = 'height'),
#       "\n")
#
#   if (!is.leaf(node)) {
#     for (child in node) {
#       preOrderIteration( child )
#     }
#   }
# }
#
# preOrderIteration(dendrogram)
# preOrderIteration(leaf)

#' A process for dendrogram object  to GraphicTree instance.
#'
#' @description
#' Convert the dendrogram object to the GraphicTree object.
#'
#' @param dendrogram
#'
#' @return  a GraphicTree instance
#' @export
#'
#' @examples
#' hc <- hclust(dist(USArrests), "ave")
#' dendrogram <- as.dendrogram(hc)
#' intuitiveTree <- process_dendrogram2intuitiveTree(dendrogram)
#' displayTheTree(intuitiveTree$rootNode)
process_dendrogram2intuitiveTree <- function(dendrogram){
  leaf_id_index <- 1;

  root_height <- attr(x = dendrogram, which = 'height');

  convert_dendrogram2intuitiveTree <- function(node, parent_height = 0) {
    # cat("Node: ",
    #     attr(x = node, which = 'members') ,
    #     "\t" ,
    #     attr(x = node, which = 'height'),
    #     "\n")

    height_currentNode <- attr(x = node, which = 'height');
    branchLength_currentNode <- parent_height - height_currentNode;


    new_node <- GraphicNode$new(leaf_id_index);
    new_node$branchLength <- branchLength_currentNode;

    if (is.leaf(node)) {
      leaf_id_index <<- leaf_id_index + 1;
      new_node$label <- attr(x = node, which = 'label')
    }else {
      for (child in node) {
        new_child <- convert_dendrogram2intuitiveTree( child, height_currentNode)
        new_node$addChild(new_child);
      }
    }

    return(new_node);
  }

  root <- convert_dendrogram2intuitiveTree(dendrogram, root_height);
  assignTheCGBID(root)

  tree = GraphicTree$new(root)
  # don not forget to minus one
  tree$numOfLeaves <- leaf_id_index - 1;
  tree$longestDepth <- root_height;

  return(tree)
}

