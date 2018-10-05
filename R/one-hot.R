#' One-hot encoding
#'
#' One-hot encoding (index variables, for statisticians) is used with
#' multi-label data, with the goal of fitting a multi-class model. Each class
#' has its own prediction probability; a threshold is then used to determine
#' which classes are most likely to apply to a new image.
#'
#' These functions assume that the data are formatted as follows:
#' \itemize{
#' \item Each file is labeled: label1_label2_label3-file-identifier, where
#'         labels are alphanumeric.
#' \item All files are to be randomly sorted into training, test, and
#'         validation sets, using the `split_prob` vector as sampling weights
#' }
#'
NULL

#' Default augmented image generator
#'
#' This generator uses samplewise std normalization, a rotation range of 40 deg,
#' a width and height shift range of 0.05, a shear range of 60 deg, a zoom range
#' of .1, a channel shift range of .1, zca whitening, and horizontal and
#' vertical flips.
#'
#' @return function to generate augmented images
#' @importFrom keras image_data_generator
aug_default <- function() {
  image_data_generator(
    samplewise_std_normalization = T,
    rotation_range = 40,
    width_shift_range = 0.05,
    height_shift_range = 0.05,
    shear_range = 60,
    zoom_range = 0.1,
    channel_shift_range = .1,
    zca_whitening = T,
    vertical_flip = T,
    horizontal_flip = TRUE
  )
}

#' Default class splitter
#'
#' @param x file name
#' @return vector of classes
#' @importFrom stringr str_remove str_replace str_split
class_split <- function(x) {
  x %>%
    stringr::str_remove_all("-.*$") %>%
    stringr::str_remove_all("^\\d{1,}\\+") %>%
    stringr::str_split("_")
}

#' Split and Augment
#'
#' @param image_dir directory of images to use to fit the model. Augmented
#'          images will be saved into this directory.
#' @param split_prob vector used to split data into training, test, and
#'          validation sets. Should add up to one. c(.5, .25, .25) is used
#'          by default.
#' @param read_classes function to split classes. Should take a file name and
#'          return a list of classes. Defaults to strsplit(x, "_")
#' @param balance_training_classes Should training classes be approximately
#'          balanced? If so, training data will be split from test and validation
#'          data, then equal numbers of images from each class will be sampled.
#'          If `balance_by_augmentation` is true, then n images with each class
#'          label are sampled, where n is the 25th percentile of class label
#'          counts. If `balance_by_augmentation` is false, n images with each
#'          class label are sampled, with total sampling probability equal to
#'          1 for
#' @param balance_by_augmentation If training classes are balanced, should
#'          images which are sampled multiple times be augmented in proportion
#'          to the number of times the image was selected?
#' @param aug_generator image_data_generator from keras
#' @param aug_times number of times to augment each image (by default). If
#'          `balance_by_augmentation` is true, then some images may be augmented
#'          more than `aug_times`.
#' @return a list with 3 components: training, test, and validation. Each vector
#'          will have a list of file names of images to be used
#' @importFrom dplyr data_frame mutate
#' @importFrom purrr map
#' @importFrom tidyr unnest
image_split_augment <- function(image_dir, split_prob = c(.5, .25, .25),
                                read_classes = class_split,
                                classes = NULL,
                                balance_training_classes = T,
                                balance_by_augmentation = T,
                                aug_generator = aug_default,
                                aug_times = 3) {

  # For internal debugging purposes...
  if (!exists("split_prob")) split_prob <- c(.5, .25, .25)
  if (!exists("read_classes")) read_classes <- class_split
  if (!exists("balance_training_classes")) balance_training_classes <- T
  if (!exists("balance_by_augmentation")) balance_by_augmentation <- T
  if (!exists("aug_generator")) aug_generator <- aug_default
  if (!exists("aug_times")) aug_times <- 3

  imgs <- dplyr::data_frame(
    file = list.files(image_dir, full.names = F, recursive = T),
    cat = sample(c("train-all", "test", "valid"),
                   size = length(file),
                   replace = T)
  )

  if (balance_training_classes) {
    imgs <- dplyr::mutate(
      imgs,
      imclass = class_split(file)) %>%
      tidyr::unnest() %>%
      group_by(file) %>%
      mutate(n_labels = n()) %>%
      ungroup()

    if (is.null(classes)) {
      # Select 90% of all classes
    }
  }




}