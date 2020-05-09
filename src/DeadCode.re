open DeadCommon;

let processSignature = (~doValues, ~doTypes, signature: Types.signature) => {
  signature
  |> List.iter(sig_item =>
       DeadValue.processSignatureItem(
         ~doValues,
         ~doTypes,
         ~path=[Common.currentModuleName^],
         sig_item,
       )
     );
};

let processCmt = (~cmtFilePath, cmt_infos: Cmt_format.cmt_infos) =>
  switch (cmt_infos.cmt_annots) {
  | Interface(signature) =>
    ProcessDeadAnnotations.signature(signature);
    processSignature(~doValues=true, ~doTypes=true, signature.sig_type);
  | Implementation(structure) =>
    let cmtiExists =
      Sys.file_exists((cmtFilePath |> Filename.remove_extension) ++ ".cmti");
    ProcessDeadAnnotations.structure(~doGenType=!cmtiExists, structure);
    processSignature(~doValues=true, ~doTypes=false, structure.str_type);
    DeadValue.processStructure(
      ~doTypes=true,
      ~doValues=true,
      ~cmt_value_dependencies=cmt_infos.cmt_value_dependencies,
      structure,
    );
  | _ => ()
  };