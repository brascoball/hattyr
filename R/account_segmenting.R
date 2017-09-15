#' Add asset type
#' 
#' Marks assets according to \url{https://mojo.redhat.com/docs/DOC-1005041}.
#' Can be then used to filter out assets by type, or to mark accounts as a
#' type.
#'
#' @param assets The assets dataframe to be updated. Requires a 
#' \code{description} and a \code{product_support_type} column
#' @param filter If not the default of NULL, it can contain one or many 
#' of the following: 'free', 'academic', 'L3', 'employee', 'self-support',
#' 'dev suite', 'partner', 'customer'
#'
#' @return an assets data frame with the new columns of asset_type and asset_type_num
#'
#' @export
add_asset_type <- function(assets, filter = NULL) {
  # remove rows with missing dates
  assets <- assets[!is.na(assets$startdate) & !is.na(assets$enddate),]
  assets$asset_type = NA

  #keys default to looking in description, unless marked
  free.keys <- c('beta', 'day', 'eval', 'preview', 
                 'unsupported', 'download and kbase only', 
                 'free', '6.?month')
  self.keys <- list(description = 'self', product_support_type = 'self-support')
  l3.keys <- 'l3'
  empl.keys <- 'employee'
  acad.keys <- c('academic', 'caudit')
  prtr.keys <- c(' nfr', '\\(nfr', 'ccp', 'rhui', 'partner', 'provider', 
                 'update infrastructure')
  rdev.keys <- c('red hat enterprise linux developer suite')
  
  keyword.list <- list(`dev suite` = rdev.keys, # dev suite are definitely this type
                       employee = empl.keys, # marks RHUI Employee and Employee evals before mismarked
                       partner = prtr.keys, # marks partner L3/unsupported/evals before mismarked
                       L3 = l3.keys, # marks non-partner L3s self-support before mismarked
                       academic = acad.keys, # marks academic self-support before mismarked
                       `self-support` = self.keys, # marks free self-support before mismarked
                       free = free.keys) # marks remaining free as free

  
  for (key in names(keyword.list)) {
    if (class(keyword.list[[key]]) == 'list') {
      for (subkey in names(keyword.list[[key]])) {
        key.grep <- paste(keyword.list[[key]][[subkey]], collapse='|')
        assets[is.na(assets$asset_type) & grepl(key.grep, tolower(assets[,subkey])), "asset_type"] <- key
      }
    } else {
      key.grep <- paste(keyword.list[[key]], collapse='|')
      assets[is.na(assets$asset_type) & grepl(key.grep, tolower(assets$description)), "asset_type"] <- key
    }
    print(paste0("after ", key, ":"))
    print(table(assets$asset_type))
  }
  
  # anything not marked yet is considered a customer asset
  assets[is.na(assets$asset_type), "asset_type"] <- 'customer'

  # add a priority for asset type in which to mark the account  
  asset_types = c("free", "academic", "L3", "employee",
                  "self-support", "dev suite", "partner", "customer")
  asset_type_to_num <- data.frame(asset_type = asset_types, asset_type_num = 1:8)
  assets$asset_type_num <- asset_type_to_num$asset_type_num[match(assets$asset_type,asset_type_to_num$asset_type)]

  # if there is a filter, filter out assets of that asset_type
  if (!is.null(filter)) assets <- assets[!(assets$asset_type %in% filter), ]
  
  return(assets)
}

#' Add product moniker
#' 
#' Marks assets according to \url{https://pnt.redhat.com/pnt/p-557504/brandarch_2pager_0916EE.pdf}.
#' Can be then used to segment assets by moniker, or to mark accounts as a
#' type.
#'
#' @param assets The assets dataframe to be updated. Requires a 
#' \code{product_group_detail} column
#' @param filter If not the default of NULL, it can contain one or many 
#' of the following: 'emerging', 'middleware', 'platform', 'other'
#'
#' @return an assets data frame with the new column of product_moniker
#'
#' @export
add_asset_moniker <- function(assets, filter = NULL) {
  # remove rows with missing dates
  assets$moniker = NA
  
  #keys default to looking in description, unless marked
  emerging <- c('ANSIBLE', 'AUTOMATION', 
                'CLOUDFORMS',
                'INTEGRATION', 'MOBILE', 'OPENSHIFT', 
                'RHCI', 'RHEL - OSP', 'RHT STORAGE')
  middleware <- c('ACCELERATION')
  platform <- c('DIRECTORY & CERTIFICATE', 'RHEL', 
                'RHEL WITH SMART VIRTUALIZATION', 'RHEV',
                'SATELLITE/SMART MGMT/INSIGHTS')
  other <- c('TRAINING (CLOUD)', 'TRAINING (MIDDLEWARE)',
             'TRAINING (OPENSHIFT)', 'TRAINING (PLATFORM)',
             'TRAINING (STORAGE)')
  
  moniker.list <- list(Emerging = emerging, Middleware = middleware,
                       Platform = platform, Other = other)
  
  for (moniker in names(moniker.list)) {
    assets[assets$product_group_detail %in% moniker.list[[moniker]], "product_moniker"] <- moniker
    print(paste0("after ", moniker, ":"))
    print(table(assets$product_moniker))
  }
  
  # if there is a filter, filter out assets of that asset_type
  if (!is.null(filter)) assets <- assets[!(assets$product_moniker %in% filter), ]
  
  return(assets)
}
