#download_data
download_plan <- list(

  # vegetation
  tar_target(
    name = cover_download,
    command =  get_file(node = "pk4bg",
                        file = "Three-D_clean_cover_2019-2022.csv",
                        path = "data",
                        remote_path = "Vegetation"),
  format = "file"
  ),

  # traits
  tar_target(
    name = trait_download,
    command =  get_file(node = "fcbw4",
                        file = "PFTC6_ThreeD_clean_leaf_traits_2022.csv",
                        path = "data",
                        remote_path = "trait_data"),
    format = "file"
  ),

  # meta data
  tar_target(
    name = metaTurfID,
    command = create_threed_meta_data()
  ),

  # cover
  tar_target(
    name = cover_raw,
    command = read_csv(cover_download)
  ),

  # sp list
  tar_target(
    name = sp_list,
    command = read_csv(file = "data/Three-D_clean_taxonomy.csv")
  ),

  # community structure
  tar_target(
    name = trait_raw,
    #command = read_csv(trait_download)
    command = read_csv(file = "data/PFTC6_ThreeD_clean_leaf_traits_2022.csv")
  )

)

