# -*- coding: utf-8 -*-
"""
Created on Wed Jan 22 09:52:40 2020

@author: Jason
"""

# import parser from tika
from tika import parser
import dirfind
 

# Set working directory
dropbox_dir = dirfind.guess_dropbox_dir()
data_dir = dropbox_dir + "/kraus/data/direktori_industri/pdf/pre2008/"

# get a sample pdf
parsedPDF = parser.from_file(data_dir + "Direktori_Industri_Pengolahan_Indonesia_2006_pages-56-72.pdf",xmlContent=True)
 
# print parsed PDF data
parsedPDF["content"]
