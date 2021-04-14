# Randomised Complete Block Design
# Delete file if it exists
if (file.exists("crd_design.csv")) file.remove("crd_design.csv")
if (file.exists("crd_design.pdf")) file.remove("crd_design.pdf")
if (file.exists("crd_design.png")) file.remove("crd_design.png")
if (file.exists("crd_design.jpeg")) file.remove("crd_design.jpeg")
if (file.exists("testfile.pdf")) file.remove("testfile.pdf")
if (file.exists("testfile.csv")) file.remove("testfile.csv")

if(!is.null(dev.list())) {dev.off()}
if (file.exists("Rplots.pdf")) file.remove("Rplots.pdf")
