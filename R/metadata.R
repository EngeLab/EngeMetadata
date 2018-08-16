#' metadata
#'
#' Downloads and processes a metadata, formated according to a standard
#' template, for a specific plate and path on Google Drive.
#'
#' @name metadata
#' @rdname metadata
#' @param plate Character; Corresponds to the plate to process metadata for.
#' Must correspond to a file name in the path argument.
#' @param path Character; The location of the plate metadata on Google Drive.
#' @return A tibble with the metadata specified in the file.
#' @author Jason Serviss
#' @examples
#'
#' \dontrun{metadata("test1", 'Enge_lab/GFP_mouse/Annotation/package_testing')}
#'
NULL
#' @export

metadata <- function(
  plate,
  path = 'Enge_lab/GFP_mouse/Annotation/package_testing'
){
  meta <- getPlateMeta(plate, path)
  checkMeta(meta)
  resolvePlateMeta(meta)
}

#' checkMeta
#'
#' Checks that assumptions concerning the metadata in Google Drive are fufilled.
#'
#' @name checkMeta
#' @keywords internal
#' @rdname checkMeta
#' @param meta List; Length 3 list with the Wells, Columns, and Plate sheets,
#' respectivley.
#' @return Nothing.
#' @author Jason Serviss
NULL

checkMeta <- function(meta) {
  #check that unique_key exists
  if(!"unique_key" %in% colnames(meta[[3]])) {
    stop("unique_key key is missing from Plate sheet.")
  }
  #check that file name matches unique key
  if(unique(names(meta)) != pull(meta[[3]], unique_key)) {
    stop("File name and unique_key do not match.")
  }
  #check that the wells_in_plate variable is present in the Plate sheet
  if(!"wells_in_plate" %in% colnames(meta[[3]])) {
    stop("wells_in_plate key missing from Plate sheet")
  }
  #check that wells_in_plate is wither 384 or 96
  if(!pull(meta[[3]], wells_in_plate) %in% c(384, 96)) {
    stop("wells_in_plate key must equal 384 or 96")
  }
  #check that the Column key in the Columns sheet is present.
  if(!"Column" %in% colnames(meta[[2]])) {
    stop("The Column key in the Columns sheet is missing.")
  }
  #check that the Column key in the Columns sheet is correct.
  wells <- pull(meta[[3]], wells_in_plate)
  if(!identical(unique(.layout(wells)$Column), meta[[2]]$Column)) {
    mess <- paste0(
      "The Column key in the Columns sheet is malformated. ", 
      "The plate format is: ", wells,
      " so Column should contain: ", 
      paste(unique(.layout(wells)$Column), collapse = ", ")
    )
    stop(mess)
  }
  #check that the Wells key in the Wells sheet is present.
  if(!"Well" %in% colnames(meta[[1]])) {
    stop("The Wells key in the Wells sheet is missing.")
  }
  #check that the Wells key in the Wells sheet is correct.
  if(!identical(.layout(wells)$Well, meta[[1]]$Well)) {
    mess <- paste0(
      "The Well key in the Wells sheet is malformated. ", 
      "The plate format is ", wells,
      " so Well should contain: ", paste(.layout(wells)$Well, collapse = ", ")
    )
    stop(mess)
  }
}

#' getPlateMeta
#'
#' Downloads and reads the metadata into R.
#'
#' @name getPlateMeta
#' @keywords internal
#' @rdname getPlateMeta
#' @param plate Character; Corresponds to the plate to process metadata for.
#' Must correspond to a file name in the path argument.
#' @param path Character; The location of the plate metadata on Google Drive.
#' @return List; Length 3 list with the Wells, Columns, and Plate sheets,
#' respectivley.
#' @author Jason Serviss
NULL
#' @importFrom dplyr pull
#' @importFrom googledrive drive_ls drive_download
#' @importFrom purrr map
#' @importFrom readxl read_excel
#' @importFrom tidyr spread

getPlateMeta <- function(
  plate, path = 'Enge_lab/GFP_mouse/Annotation/package_testing'
){
  files <- pull(drive_ls(path = path), name)
  if(!plate %in% files) stop(paste0(plate, " not found on Google Drive."))

  #download plate data
  tmp <- tempdir()
  p <- file.path(tmp, paste0(plate, ".xlsx"))
  drive_download(
    plate, path = p, overwrite = TRUE
  )
  
  if(!all(c("Plate", "Columns", "Wells") %in% excel_sheets(p))) {
    s <- c("Plate", "Columns", "Wells")
    missing <- s[which(!s %in% excel_sheets(p))]
    stop(paste0(missing, " is not present in the sheets of plate ", p))
  }
  
  #read and extract data
  meta <- map(c("Wells", "Columns", "Plate"), function(s) {
    d <- read_excel(path = p, sheet = s)
    if(s == "Plate") d <- spread(d, Key, Value, convert = TRUE)
    d
  })
  
  names(meta) <- rep(plate, 3)
  
  #format dates
  .dates(meta)
}

.dates <- function(meta) {
  dateCols <- map(meta, function(d) {
    colnames(d)[str_detect(colnames(d), "date")]
  })
  
  map2(meta, dateCols, function(x, y) {
    if(length(y) == 0) {x} else {
      mutate(x, !! y := lubridate::ymd(!! dplyr::sym(y)))
    }
  })
}

#' resolvePlateMeta
#'
#' Resolves the precedence of the of the keys in the metadata file with the
#' hierarchy Wells > Columns > Plates.
#'
#' @name resolvePlateMeta
#' @keywords internal
#' @rdname resolvePlateMeta
#' @param meta List; Length 3 list with the Wells, Columns, and Plate sheets,
#' respectivley. Typically from the \code{\link{getPlateMeta}} function.
#' @return A tibble with the resolved metadata.
#' @author Jason Serviss
NULL
#' @importFrom dplyr pull bind_cols full_join select matches
#' @importFrom purrr map_dfr map_dfc

resolvePlateMeta <- function(meta) {
  #layout plate
  wells <- pull(meta[[3]], wells_in_plate)
  layout <- .layout(wells)

  #Add plate data to layout
  base <- map_dfr(1:nrow(layout), function(x) meta[[3]]) %>%
    bind_cols(layout, .)

  #resolve Plate and Column prescedence
  bind1 <- full_join(base, meta[[2]], by = "Column")
  keys1 <- .processKeys(colnames(bind1))
  add1 <- map_dfc(keys1,  ~.resolve(bind1, .x))
  full1 <- bind_cols(bind1, add1)
  resolved1 <- select(full1, -matches("\\.[x-y]"))

  #resolve Well and combined Plate and Column prescedence
  bind2 <- full_join(resolved1, meta[[1]], by = "Well")
  keys2 <- .processKeys(colnames(bind2))
  add2 <- map_dfc(keys2,  ~.resolve(bind2, .x))
  full2 <- bind_cols(bind2, add2)
  resolved2 <- select(full2, -matches("\\.[x-y]"))

  return(resolved2)
}

#' .processKeys
#'
#' Helper for \code{\link{resolvePlateMeta}}.Extracts duplicated key names with
#' a ".x" or ".y" suffix.
#'
#' @name .processKeys
#' @keywords internal
#' @param data A tibble with potentially dupicated keys in the colnames.
#' @return A character vector of the duplicated keys without their suffix.
#' @author Jason Serviss
NULL
#' @importFrom stringr str_replace

.processKeys <- function(keys) {
  kp <- str_replace(keys, "(.*)\\.[x-y]", "\\1")
  kp[duplicated(kp)]
}

#' .resolve
#'
#' Helper for \code{\link{resolvePlateMeta}}. Resolved key precedence.
#' Expects the greater suffix (i.e. .y > .x) to have greater precedence.
#' Expects only .x and .y suffixes.
#'
#' @name .resolve
#' @keywords internal
#' @param data Tibble; including the keys to be resolved.
#' @param key Character; the key to be resolved. key.x and key.y are expected
#' in colnames(data).
#' @return A character vector of the duplicated keys without their suffix.
#' @author Jason Serviss
NULL
#' @importFrom dplyr select mutate sym
#' @importFrom rlang ":=" "!!"
#' @importFrom purrr map2
#' @importFrom tidyr unnest

.resolve <- function(data, key) {
  if(length(key) == 0) return(data)

  key.x <- paste0(key, ".x"); key.y <- paste0(key, ".y")
  key.x.sym <- sym(key.x); key.y.sym <- sym(key.y)

  if(!all(c(key.x, key.y) %in% colnames(data))) {
    stop("Keys missing from the data.")
  }

  data %>%
    select(key.x, key.y) %>%
    mutate(!! key := map2(!! key.y.sym, !! key.x.sym, function(y, x) {
      replace(y, is.na(y), x)
    })) %>%
    select(-key.x, -key.y) %>%
    unnest()
}

#' .layout
#'
#' Helper for \code{\link{resolvePlateMeta}}. Sets up the plate layout.
#'
#' @name .layout
#' @keywords internal
#' @param format Numeric; Indicates which plate to setup. Can be 384 or 96.
#' @return A tibble with the plate layout.
#' @author Jason Serviss
NULL
#' @importFrom tibble tibble is_tibble

.layout <- function(format) {
  if(format == 384) {
    r <- LETTERS[1:16]
    c <- c(paste0("0", 1:9), 10:24)
  } else {
    r <- LETTERS[1:8]
    c <- c(paste0("0", 1:9), 10:12)
  }

  tibble(
    Row = rep(r, each = length(c)),
    Column = rep(c, length(r)),
    Well = paste0(Row, Column)
  )
}

