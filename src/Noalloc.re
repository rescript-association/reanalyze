let processCmt = (cmt_infos: Cmt_format.cmt_infos) =>
  switch (cmt_infos.cmt_annots) {
  | Interface(_) => ()
  | Implementation(_) =>
    Log_.item(
      "Noalloc running on %s@.",
      Common.currentSrc^ |> Filename.basename,
    )
  | _ => ()
  };

let reportResults = (~ppf as _) => ();
