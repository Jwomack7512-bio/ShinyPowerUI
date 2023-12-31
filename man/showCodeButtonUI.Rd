% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/showCodeButtonUI.R
\name{showCodeButtonUI}
\alias{showCodeButtonUI}
\title{UI function to create a show/hide code button}
\usage{
showCodeButtonUI(
  id,
  hyperlink_color = "blue",
  background_color = "transparent",
  border_style = "none",
  button_alignment = "left"
)
}
\arguments{
\item{id}{A unique identifier for the UI elements.}

\item{hyperlink_color}{Color of the hyperlink text (default: "blue").}

\item{background_color}{Background color of the button (default: "transparent").}

\item{border_style}{CSS style for the button border (default: "none").}

\item{button_alignment}{where in column to align button (left, center, right)}
}
\value{
A Shiny UI element.
}
\description{
This function generates the UI components for displaying a button that
toggles the visibility of generated code.
}
