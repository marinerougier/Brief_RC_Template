# LIBRARIES ----------------------------------------------------------------------------------------
#' @importFrom png writePNG readPNG
#' @importFrom jpeg readJPEG
#' @importFrom magick image_read
#' @importFrom imager width height Xc Yc

# FUNCTIONS ----------------------------------------------------------------------------------------
#' @title Read and scale base image
#' @description Read and scale the base image
#' @param baseImgPath String specifying path to the base image.
#' @return Vector of pixels from the base image.
#' @examples NULL
#' @export readBaseImg
readBaseImg <- function(baseImgPath, maxContrast = TRUE) {
    # Read image
    if (grepl('png|PNG', baseImgPath)) {
        baseImg <- png::readPNG(baseImgPath)
    } else {
        baseImg <- jpeg::readJPEG(baseImgPath)
    }

    # Ensure there is only 2 dimensions
    if (length(dim(baseImg)) == 3) baseImg <- baseImg[, , 1]

    # Maximize base image contrast
    if (maxContrast) baseImg <- (baseImg - min(baseImg))/(max(baseImg) - min(baseImg))

    return( as.vector(baseImg) )
}

#' @title Generate (and scale) mask from responses
#' @description Generate (and scale) mask from responses.
#' @param response Numerical vector specifying the reponses.
#' @param stim Numerical vector specifying the stimuli number.
#' @param noiseMatrix Matrix of noise pattern as generated with
#'   \code{noiseMatrix <- rcirc::generateStimuli2IFC(..., return_as_dataframe = TRUE)}.
#' @param baseImg Numerical vector containing the baseImg image or string pointing to the baseImg
#'   image file. If baseImg is a string, then the baseImg image must in .png or .jpeg.
#' @param scaling String|Scalar|NULL specifying the scaling method. `"matched"` is the default method.
#'   If a scalar is provided (e.g. 5) than the `"constant"` method will be applied.
#'   If `NULL` no scaling is applied.
#' @return List with the (un)scaled Noise mask (\code{$mask}) and the base image as a vector
#'   (\code{$baseImgVect}).
#' @examples NULL
#' @export genMask
genMask <- function(response, stim, noiseMatrix, baseImg, scaling = "matched") {
    # Generate mask
    X  <- data.table::data.table(response = response, stim = stim)
    X  <- X[, .(response = mean(response)), stim]
    mask <- (noiseMatrix[, X$stim] %*% X$response) / length(X$response)

    # Read base image
    if (is.character(baseImg)) baseImg <- readBaseImg(baseImg)

    # Scale mask
    if (scaling == "matched") {
        scaledMask <- min(baseImg)+((max(baseImg)-min(baseImg))*(mask-min(mask))/(max(mask)-min(mask)))
    } else if (is.numeric(scaling)) { # constant scaling
        scaledMask <- (mask + scaling)/(2 * scaling)
        if (max(scaledMask) > 1 | min(scaledMask) < -1) warning("Constant is too low! Adjust.")
    } else if (is.null(scaling)) { # No scaling
        scaledMask <- mask
    }

    return(list(mask = scaledMask, baseImgVect = baseImg))
}

#' @title Generate Classification Image (CI)
#' @description Generate the combinaed of the noise mask (mask) and the base image.
#' @inheritParams genMask
#' @param outputPath String specifying the file path of the ouput CI. Default is `"combined.png"`.
#' @return NULL
#' @examples NULL
#' @export genCI
genCI <- function(response, stim, noiseMatrix, baseImg, scaling = "matched",
                  outputPath = "combined.png") {
    # Generate mask
    M <- genMask(response, stim, noiseMatrix, baseImg, scaling)
    mask <- M$mask
    baseImgVect <- M$baseImgVect

    # Write and save combined image
    combined <- (baseImgVect + mask) / 2
    combined <- matrix(combined, nrow = 512)
    png::writePNG(combined, outputPath)

    # Return file path
    invisible(outputPath)
}


#' @title Get face region
#' @description Returns a logical vector with the face region
#' @param imgPath String specifying path to the base image.
#' @param xpos Numeric specifiying the X position (relative to the center).
#' @param ypos Numeric specifiying the Y position (relative to the center).
#' @param faceWidth Numeric specifiying the width of the face region.
#' @param faceHeight Numeric specifiying the height of the face region.
#' @param preview Numeric specifiying the height of the face region.
#' @param writeImgTo String specifying where and if the output image should be saved. Default is
#'   NULL, meaning that the image will not be saved.
#' @return Logical vector specifying the location of the face region
#' @examples NULL
#' @export getFaceRegion
getFaceRegion <- function(imgPath,
                          xpos = 0, ypos = 0, faceWidth = 1.4, faceHeight = 1.8,
                          preview = TRUE, writeImgTo = NULL) {
    # Read image and convert to matrix
    face <- readBaseImg(imgPath)
    faceLength <- sqrt(length(face)) # must be squared
    face <- matrix(face, ncol = faceLength)

    # Define face region: https://dahtah.github.io/imager/gimptools.html
    Xcc <- function(im) imager::Xc(im) - imager::width(im)/2  + ypos
    Ycc <- function(im) imager::Yc(im) - imager::height(im)/2 + xpos
    NN <- imager::as.cimg( matrix(1:faceLength^2, nrow = faceLength) )
    faceRegion <- (Xcc(NN)/faceHeight)^2 + (Ycc(NN)/faceWidth)^2 < 100^2
    faceRegion <- as.vector(faceRegion)

    # Preview in Viewer
    if (preview) {
        alphaMask <- matrix(1, faceLength, faceLength)
        alphaMask[!faceRegion] <- 0.6
        previewFace <- abind::abind(face, alphaMask, along = 3)
        previewFacePath <- tempfile(fileext = ".png")
        png::writePNG(previewFace, previewFacePath)
        invisible(capture.output(print(magick::image_read(previewFacePath))))
    }
    # Write face
    if (!is.null(writeImgTo)) {
        alphaMask <- matrix(1, faceLength, faceLength)
        alphaMask[!faceRegion] <- 0
        printedFace <- abind::abind(face, alphaMask, along = 3)
        png::writePNG(printedFace, writeImgTo)
    }

    invisible(faceRegion)
}
