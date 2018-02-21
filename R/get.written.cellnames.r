# gg is the output of load_ldsc_gene_groups()
get.written.cellnames <- function(files,cellnames){
    library(stringi)
    tmp = data.frame(file=files,celltypes=cellnames,writtenName=rep("",length(cellnames)),stringsAsFactors=FALSE)
    tmp$writtenName = gsub("(?!^)(?=[[:upper:]])", " ", tmp$celltypes, perl=T) #<--- I have no idea what this was doing... 
    # - It may have been for splitting up words which had been joint together... it introduces errors such as GABA to G A B A
    #tmp$writtenName = tmp$celltypes
    tmp$writtenName = stri_trans_totitle(tmp$writtenName)
    tmp$writtenName = gsub("(\\d)\\(","\\1 \\(",tmp$writtenName)
    tmp$writtenName = gsub("G A B Aergic","GABAergic",tmp$writtenName)
    tmp$writtenName = gsub("SS","(SS)",tmp$writtenName)
    tmp$writtenName = gsub("CA1","(CA1)",tmp$writtenName)
    tmp$writtenName = gsub("CA3","(CA3)",tmp$writtenName)
    tmp$writtenName = gsub("CA2","(CA2)",tmp$writtenName)
    tmp$writtenName = gsub("S S","(SS)",tmp$writtenName)
    tmp$writtenName = gsub("C A1","(CA1)",tmp$writtenName)
    tmp$writtenName = gsub("C A3","(CA3)",tmp$writtenName)
    tmp$writtenName = gsub("C A2","(CA2)",tmp$writtenName)
    
    tmp$writtenName = gsub("D1 R","D1R",tmp$writtenName)
    tmp$writtenName = gsub("D2 R","D2R",tmp$writtenName)
    tmp$writtenName = gsub("C H A T","CHAT",tmp$writtenName)
    tmp$writtenName = gsub("N P Y- N G C","NPY-NGC",tmp$writtenName)
    tmp$writtenName = gsub("Radialglialikecells","Radial Glia Like ",tmp$writtenName)
    tmp$writtenName = gsub("- Ventraltegmentalarea(\\d)"," (VTA\\1)",tmp$writtenName)
    tmp$writtenName = gsub("-Ventral Tegmental Area(\\d)"," (VTA\\1)",tmp$writtenName)
    tmp$writtenName = gsub("- Substantianigra"," (SNc)",tmp$writtenName)
    tmp$writtenName = gsub("-Substantia Nigra"," (SNc)",tmp$writtenName)
    tmp$writtenName = gsub(";"," ",tmp$writtenName)
    tmp$writtenName = gsub("\\(VMH\\)","VMH",tmp$writtenName)
    tmp$writtenName = gsub("\\(Vmh\\)","VMH",tmp$writtenName)
    tmp$writtenName = gsub("\\(Pvh\\)","PVH",tmp$writtenName)
    tmp$writtenName = gsub("\\(Sch\\)","Sch",tmp$writtenName)
    tmp$writtenName = gsub("\\(Pvh-Pvh-Galo\\)","PVH- PVH-Galo",tmp$writtenName)
    tmp$writtenName = gsub("\\(Bst Mpo\\)","BST MPO",tmp$writtenName)
    tmp$writtenName = gsub("\\(BST MPO\\)","BST MPO",tmp$writtenName)
    tmp$writtenName = gsub("\\(Arh\\)","Arh",tmp$writtenName)
    
    tmp$writtenName = gsub("Gaba","GABA",tmp$writtenName)
    #tmp$writtenName = gsub("(\\d)(\\w)$","\\1\\L\\2",tmp$writtenName,perl=TRUE) # If final two characters are something like "1C" then make last letter lowercase
    tmp$writtenName = gsub("(\\d)([ABCDEFG])$","\\1\\L\\2",tmp$writtenName,perl=TRUE) # If final two characters are something like "1C" then make last letter lowercase
    #tmp$writtenName = gsub("^Int(\\d*)$","Interneuron \\1",tmp$writtenName)
    tmp$writtenName = gsub("Int(1|2|4)$","Cortical Interneuron \\1 (Sst)",tmp$writtenName)
    tmp$writtenName = gsub("Int Pvalb","Cortical Interneuron (Pvalb)",tmp$writtenName)
    tmp$writtenName = gsub("Int(6|9|10)$","Cortical Interneuron \\1 (Vip)",tmp$writtenName)
    tmp$writtenName = gsub("Int13$","Cortical Interneuron 13 (Reln Igtp)",tmp$writtenName)
    tmp$writtenName = gsub("Int14$","Cortical Interneuron 14 (Reln Smad3)",tmp$writtenName)
    tmp$writtenName = gsub("Int(15|16)$","Cortical Interneuron \\1 (Reln Ndnf)",tmp$writtenName)
    tmp$writtenName = gsub("Int(5|7|8|11|12|13|14)$","Cortical Interneuron \\1 (Reln)",tmp$writtenName)
    tmp$writtenName = gsub("(Neuroblasts|Neuron)(\\d|\\d[abcde])","\\1 \\2",tmp$writtenName)
    tmp$writtenName = gsub("Oculomotorand Trochlearnucleusembryonicneurons","Motor Nucleus Neurons",tmp$writtenName)
    tmp$writtenName = gsub("Rednucleusembryonicneurons","Red Nucleus Neurons",tmp$writtenName)
    tmp$writtenName = gsub("Dopaminergic Adult","Dopaminergic Neuron",tmp$writtenName)
    tmp$writtenName = gsub("S1 Pyr (.*)","Pyramidal Neuron (SS \\1)",tmp$writtenName)
    tmp$writtenName = gsub("S1pyrl(.*)","Pyramidal Neuron (SS L\\1)",tmp$writtenName)
    tmp$writtenName = gsub("S1pyr(.*)","Pyramidal Neuron (SS \\1)",tmp$writtenName)
    tmp$writtenName = gsub("D L","DL",tmp$writtenName)
    tmp$writtenName = gsub("SS dl","SS DL",tmp$writtenName)
    tmp$writtenName = gsub("Pyramidal Ss","Pyramidal SS",tmp$writtenName)
    tmp$writtenName = gsub("Pyramidal Ca","Pyramidal CA",tmp$writtenName)
    tmp$writtenName = gsub("Oligo(\\d)","Oligodendrocyte \\1",tmp$writtenName)
    tmp$writtenName = gsub("Mgl(\\d)","Microglia \\1",tmp$writtenName)
    tmp$writtenName = gsub("Pvm(\\d)","Perivascular Macrophages \\1",tmp$writtenName)
    tmp$writtenName = gsub("Vsmc","Vasc. Smooth Muscle Cell",tmp$writtenName)
    tmp$writtenName = gsub("Vend(\\d)","Vasc. Endothelial \\1",tmp$writtenName)
    tmp$writtenName = gsub("^Peric$","Pericyte",tmp$writtenName)
    tmp$writtenName = gsub("Astro(\\d)","Astrocyte \\1",tmp$writtenName)
    tmp$writtenName = gsub("Epend","Ependymal",tmp$writtenName)
    tmp$writtenName[grep("SCAPT|RPKM",tmp$file)] = gsub("Int(\\d) (.*)","Interneuron \\1 (\\2)",tmp$writtenName[grep("SCAPT|RPKM",tmp$file)])
    tmp$writtenName[grep("SCAPT|RPKM",tmp$file)] = gsub("Pyr(\\d) (.*)","Pyramidal Neuron \\1 (\\2)",tmp$writtenName[grep("SCAPT|RPKM",tmp$file)])
    tmp$writtenName = gsub("2and3","2/3",tmp$writtenName)
    tmp$writtenName[grep("TASIC|Tasic",tmp$file)] = gsub("^Astro (.*)","Astrocytes (\\1)",tmp$writtenName[grep("TASIC|Tasic",tmp$file)])
    tmp$writtenName[grep("TASIC|Tasic",tmp$file)] = gsub("^(Vip .*)","Interneurons (\\1)",tmp$writtenName[grep("TASIC|Tasic",tmp$file)])
    tmp$writtenName[grep("TASIC|Tasic",tmp$file)] = gsub("^(Sst .*)","Interneurons (\\1)",tmp$writtenName[grep("TASIC|Tasic",tmp$file)])
    tmp$writtenName[grep("TASIC|Tasic",tmp$file)] = gsub("^Sncg","Interneurons (Sncg)",tmp$writtenName[grep("TASIC|Tasic",tmp$file)])
    tmp$writtenName[grep("TASIC|Tasic",tmp$file)] = gsub("^Smad3(.*)","Interneurons (Smad3\\1)",tmp$writtenName[grep("TASIC|Tasic",tmp$file)])
    tmp$writtenName[grep("TASIC|Tasic",tmp$file)] = gsub("^(Pvalb.*)","Interneurons (\\1)",tmp$writtenName[grep("TASIC|Tasic",tmp$file)])
    tmp$writtenName[grep("TASIC|Tasic",tmp$file)] = gsub("^O P C (.*)","Oligodendrocyte Precursor",tmp$writtenName[grep("TASIC|Tasic",tmp$file)])
    tmp$writtenName[grep("TASIC|Tasic",tmp$file)] = gsub("^Oligo (.*)","Oligodendrocyte (\\1)",tmp$writtenName[grep("TASIC|Tasic",tmp$file)])
    tmp$writtenName[grep("TASIC|Tasic",tmp$file)] = gsub("^(Ndnf .*)","Interneurons (\\1)",tmp$writtenName[grep("TASIC|Tasic",tmp$file)])
    tmp$writtenName[grep("TASIC|Tasic",tmp$file)] = gsub("^Igtp","Interneurons (Igtp)",tmp$writtenName[grep("TASIC|Tasic",tmp$file)])
    tmp$writtenName[grep("TASIC|Tasic",tmp$file)] = gsub("^Endo (.*)","Endothelial (\\1)",tmp$writtenName[grep("TASIC|Tasic",tmp$file)])
    tmp$writtenName[grep("TASIC|Tasic",tmp$file)] = gsub("^Micro (.*)","Microglia (\\1)",tmp$writtenName[grep("TASIC|Tasic",tmp$file)])
    tmp$writtenName[grep("TASIC|Tasic",tmp$file)] = gsub("^(L\\d|L\\d[ab]) (.*)","Pyramidal Neuron \\1 (\\2)",tmp$writtenName[grep("TASIC|Tasic",tmp$file)])
    tmp$writtenName[grep("TASIC|Tasic",tmp$file)] = gsub("^L2/3 (.*)","Pyramidal Neuron L2/3 (\\1)",tmp$writtenName[grep("TASIC|Tasic",tmp$file)])
    tmp$writtenName[tmp$celltypes=="Astro"] = "Astrocytes"
    tmp$writtenName[grepl("TasicStriatum",tmp$file) & tmp$celltypes=="Astro"] = "Astrocytes (Striatal)"
    tmp$writtenName[grepl("TasicStriatum",tmp$file) & tmp$celltypes=="Astrocytes"] = "Astrocytes (Cortical)"
    tmp$writtenName[tmp$celltypes=="OPC"] = "Oligodendrocyte Precursor"
    tmp$writtenName[grepl("TasicStriatum",tmp$file) & tmp$celltypes=="OPC"] = "OPC (Striatal)"
    tmp$writtenName[grepl("TasicStriatum",tmp$file) & tmp$celltypes=="OligodendrocytePrecursorCell"] = "OPC (Cortical)"
    tmp$writtenName[tmp$celltypes=="Oligo"] = "Oligodendrocytes"
    tmp$writtenName[grepl("TasicStriatum",tmp$file) & tmp$celltypes=="Oligo"] = "Oligodendrocytes (Striatal)"
    tmp$writtenName[grepl("TasicStriatum",tmp$file) & tmp$celltypes=="Oligodendrocytes"] = "Oligodendrocytes (Cortical)"
    tmp$writtenName[tmp$celltypes=="Endothelialcells"] = "Endothelial"
    tmp$writtenName[grepl("TasicStriatum",tmp$file) & tmp$celltypes=="Vascular"] = "Endothelial (Striatal)"
    tmp$writtenName[grepl("TasicStriatum",tmp$file) & tmp$celltypes=="Endothelialcells"] = "Endothelial (Cortical)"        
    tmp$writtenName[grepl("Quake",tmp$file) & tmp$celltypes=="StriatalNeuron"] = "Medium Spiny Neurons"
    #tmp$writtenName[grep("TASIC",tmp$file)] = gsub("^(.*)"," (\\1)",tmp$writtenName[grep("TASIC",tmp$file)])
    #tmp$writtenName[grep("TASIC",tmp$file)] = gsub("^(.*)"," (\\1)",tmp$writtenName[grep("TASIC",tmp$file)])
    #tmp$writtenName[grep("TASIC",tmp$file)] = gsub("^(.*)"," (\\1)",tmp$writtenName[grep("TASIC",tmp$file)])
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    # tmp$writtenName = gsub("","",tmp$writtenName)
    tmp$writtenName[tmp$celltypes=="interneurons"] = "Cortical Interneurons"
    tmp$writtenName[tmp$celltypes=="Embryonicmidbrainnucleusneurons"] = "Embryonic Midbrain Nucleus Neurons"
    tmp$writtenName[tmp$celltypes=="astrocytes_ependymal"] = "Astrocytes / Ependymal"
    tmp$writtenName[tmp$celltypes=="ClauPyr"] = "Pyramidal Neuron (Claustrum)"
    tmp$writtenName[tmp$celltypes=="SubPyr"] = "Pyramidal Neuron (Subiculum)"
    tmp$writtenName[tmp$celltypes=="CA1Pyr1"] = "Pyramidal Neuron (CA1) 1"
    tmp$writtenName[tmp$celltypes=="CA1Pyr2"] = "Pyramidal Neuron (CA1) 2"
    tmp$writtenName[tmp$celltypes=="CA2Pyr2"] = "Pyramidal Neuron (CA2) 2"
    tmp$writtenName[tmp$celltypes=="CA1PyrInt"] = "Pyramidal Neuron (CA1) Int"
    tmp$writtenName[tmp$celltypes=="fetal_quiescent"] = "Cortical Fetal Neurons"
    tmp$writtenName[tmp$celltypes=="fetal_replicating"] = "Cortical Neuronal Progenitors"
    tmp$writtenName[grepl("mouse",tmp$file) & tmp$celltypes=="NeuralProgenitors"] = "Neuronal Progenitor"
    tmp$writtenName[grepl("Human",tmp$file) & tmp$celltypes=="NeuralProgenitors"] = "Neuron Progenitor"
    tmp$writtenName[tmp$celltypes=="Ependy-C"] = "Ependymal - ciliated"
    tmp$writtenName[tmp$celltypes=="Ependy-Sec"] = "Ependymal - secretory"
    tmp$writtenName[tmp$celltypes=="MSN"] = "Medium Spiny Neuron"
    tmp$writtenName[tmp$celltypes=="MSND1R"] = "Medium Spiny Neuron D1R"
    tmp$writtenName[tmp$celltypes=="MSND2R"] = "Medium Spiny Neuron D2R"
    tmp$writtenName[tmp$celltypes=="NSC"] = "Neuronal Stem Cells"
    tmp$writtenName[tmp$celltypes=="Oligo9630013A20Rik"] = "Oligodendrocytes (9630013A20Rik)"
    tmp$writtenName[tmp$celltypes=="AntFC"] = "Anterior Frontal"
    tmp$writtenName[tmp$celltypes=="BA11"] = "BA11"
    tmp$writtenName[tmp$celltypes=="BA20"] = "BA20"
    tmp$writtenName[tmp$celltypes=="BA37"] = "BA37"
    tmp$writtenName[tmp$celltypes=="BA38"] = "BA38"
    tmp$writtenName[tmp$celltypes=="BA39"] = "BA39"
    tmp$writtenName[tmp$celltypes=="BA4"] = "BA4"
    tmp$writtenName[tmp$celltypes=="BA41"] = "BA41"
    tmp$writtenName[tmp$celltypes=="BA44"] = "BA44"
    tmp$writtenName[tmp$celltypes=="FC"] = "Frontal Cortex"
    tmp$writtenName[tmp$celltypes=="OccBA19"] = "BA19"
    tmp$writtenName[tmp$writtenName=="PyramidalNeurons"] = "Pyramidal Neurons"
    tmp$writtenName = gsub(" $","",tmp$writtenName)
    tmp$writtenName = gsub("(\\w)\\(","\\1 \\(",tmp$writtenName)
    tmp$writtenName = gsub("\\( ","\\(",tmp$writtenName)
    tmp$writtenName = gsub("\\)(\\w) ","\\1\\) ",tmp$writtenName)
    tmp$writtenName = gsub("\\+and\\- ","+/- ",tmp$writtenName)
    tmp$writtenName = gsub("G A B A","GABA",tmp$writtenName)
    tmp$writtenName = gsub("V M A T","Vmat",tmp$writtenName)
    tmp$writtenName = gsub("V M H","VMH",tmp$writtenName)
    tmp$writtenName = gsub("P V H","PVH",tmp$writtenName)
    tmp$writtenName = gsub("V I P","Vip",tmp$writtenName)
    tmp$writtenName = gsub("S C H","SCH",tmp$writtenName)
    tmp$writtenName = gsub("P D Y N","Pdyn",tmp$writtenName)
    tmp$writtenName = gsub("A R H","Arh",tmp$writtenName)
    tmp$writtenName = gsub("A E A","AGAEA",tmp$writtenName)
    tmp$writtenName = gsub("B S T","BST",tmp$writtenName)
    tmp$writtenName = gsub("M P O","MPO",tmp$writtenName)
    tmp$writtenName = gsub("2 A G","2AG",tmp$writtenName)
    tmp$writtenName = gsub("Newlyformedoligo","Newly Formed Oligo",tmp$writtenName)
    tmp$writtenName = gsub("Myelin-Formingoligo","Myelin-forming Oligo",tmp$writtenName)
    tmp$writtenName = gsub("Matureoligo","Mature Oligo",tmp$writtenName)
    tmp$writtenName = gsub("^Oligo_precursor$","Oligodendrocyte Precursor",tmp$writtenName)
    tmp$writtenName = gsub("^Glutamatergic$","Pyramidal Neuron",tmp$writtenName)
    tmp$writtenName = gsub("^GABAergic$","Cortical Interneuron",tmp$writtenName)
    tmp$writtenName = gsub("Vascular","Vasc.",tmp$writtenName)
    tmp$writtenName = gsub("Embryonic","Embr.",tmp$writtenName)
    tmp$writtenName = gsub("Dopaminergic","DA",tmp$writtenName)
    tmp$writtenName = gsub("Hypothalamic","Hypoth.",tmp$writtenName)
    tmp$writtenName = gsub("Expressing","",tmp$writtenName)
    tmp$writtenName = gsub("Oxytocinand","Oxytocin /",tmp$writtenName)
    tmp$writtenName = gsub("Interneuronsother","Interneurons (other)",tmp$writtenName)
    tmp$writtenName = gsub("Committedoligodendrocyteprecursors","Committed OPC",tmp$writtenName)
    tmp$writtenName = gsub("Committed oligodendrocyte precursors","Committed OPC",tmp$writtenName)
    tmp$writtenName = gsub("Committed Oligodendrocyte Precursors","Committed OPC",tmp$writtenName)
    tmp$writtenName = gsub("Dopamine","DA",tmp$writtenName)
    tmp$writtenName = gsub("+And−","+/-",tmp$writtenName)
    tmp$writtenName = gsub("Neuron$","Neurons",tmp$writtenName)
    tmp$writtenName = gsub("\\.\\.","\\.",tmp$writtenName)
    tmp$writtenName = gsub("Pyramidal\\. ","Pyramidal ",tmp$writtenName)
    tmp$writtenName = gsub("Neural\\. ","Neural ",tmp$writtenName)
    tmp$writtenName = gsub("Radial\\.glia\\.like\\.cells","Radial glia like ",tmp$writtenName)
    tmp$writtenName = gsub("Endothelial\\.mural","Endothelial / mural",tmp$writtenName)
    tmp$writtenName = gsub("ergic\\. Neurons","ergic Neurons",tmp$writtenName)
    tmp$writtenName = gsub("Medium\\. Spiny\\. Neuron","Medium Spiny Neuron",tmp$writtenName)
    tmp$writtenName = gsub("Neural\\. Progenitors","Neural Progenitors",tmp$writtenName)
    tmp$writtenName = gsub("Striatal\\. ","Striatal ",tmp$writtenName)
    tmp$writtenName = gsub("Leptomeningeal\\. ","Leptomeningeal ",tmp$writtenName)
    tmp$writtenName = gsub("Oxytocin\\.and\\. Vasopressin\\. \\. Neurons","Oxytocin and Vasopressin Neurons",tmp$writtenName)
    tmp$writtenName = gsub("Oligodendrocyte\\.","Oligodendrocyte",tmp$writtenName)
    tmp$writtenName = gsub("Embr\\.midbrain\\.nucleus\\.neurons","Embr\\. midbrain nucleus neurons",tmp$writtenName)
    tmp$writtenName = gsub("Myelin\\.forming\\.oligodendrocytes\\.","Myelin forming oligodendrocytes ",tmp$writtenName)
    tmp$writtenName = gsub("Newly\\.formed\\.oligodendrocytes\\.","Newly formed oligodendrocytes ",tmp$writtenName)
    tmp$writtenName = gsub("Mediolateral\\.","Mediolateral",tmp$writtenName)
    tmp$writtenName = gsub("Mediolateral\\.","Mediolateral",tmp$writtenName)
    tmp$writtenName = gsub("Lateral\\. Neuroblasts\\.","Lateral Neuroblasts",tmp$writtenName)
    tmp$writtenName = gsub("Medial\\. Neuroblasts\\.","Medial Neuroblasts",tmp$writtenName)
    tmp$writtenName = gsub("Ventral\\.tegmental\\.area","Ventral tegmental area ",tmp$writtenName)
    tmp$writtenName = gsub("Mature\\.oligodendrocytes\\.","Mature oligodendrocytes ",tmp$writtenName)
    tmp$writtenName = gsub("\\(\\(","\\(",tmp$writtenName)
    tmp$writtenName = gsub("\\)\\)","\\)",tmp$writtenName)
    tmp$writtenName = gsub("\\.and\\."," and ",tmp$writtenName)
    tmp$writtenName = gsub("\\.Other\\."," \\(Other\\)",tmp$writtenName)
    tmp$writtenName = gsub("  "," ",tmp$writtenName)
    tmp$writtenName = gsub("D A","DA",tmp$writtenName)
    tmp$writtenName = gsub(" And "," and ",tmp$writtenName)    
    tmp$writtenName = gsub("Like Cells","Like",tmp$writtenName)    
    return(tmp$writtenName)
}