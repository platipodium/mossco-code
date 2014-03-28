#! /bin/env/python

from xml.etree import ElementTree as et
import sys
import os.path
import yaml

if len(sys.argv) > 1:
    filename = sys.argv[1]
else:
     filename = 'clm_netcdf_component.yaml'

print sys.argv, len(sys.argv)
if not os.path.exists(filename):
    print 'File ' + filename + ' does not exist.'
    exit(1)
    
print 'Using ' + filename + ' ...' 

fid = file(filename,'rU')
config = yaml.load(fid)
fid.close()

cf_xml_file='cf-standard-name-table.xml'

importlist=[]
exportlist=[]
top=config.values()[0]
if top.has_key('exports'):
    exportlist=config.values()[0]['exports'].keys()
if top.has_key('imports'):
    importlist=config.values()[0]['imports'].keys()
component={'name': config.keys()[0], 'imports' : importlist, 'exports' : exportlist}

# Determine the height of the box needed:
height=200 + 30*(len(exportlist) + len(importlist))

# create an SVG XML element (see the SVG specification for attribute details)
doc = et.Element('svg', width='280', height=str(height), version='1.1', xmlns='http://www.w3.org/2000/svg')
 
def draw_component(doc, component):
 
  titlewidth=len(component['name'])*23
  # Add the top left corner rounded rectangle where the component title goes
  et.SubElement(doc, 'rect', ry='10', y='10', x='10', height='50', width=str(titlewidth), style = 'fill:#f2f2f2;fill-opacity:1;stroke:#999999;stroke-width:2;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;stroke-dashoffset:0')

  # Add the main rounded rectangle where the component content goes
  et.SubElement(doc, 'rect', ry='10', y='40', x='5', height=str(height-60), width='270', style = 'fill:#f2f2f2;fill-opacity:1;stroke:#999999;stroke-width:2;stroke-miterlimit:4;stroke-opacity:1;stroke-dasharray:none;stroke-dashoffset:0')

  # Add the comonent title text
  text=et.Element('text', style='font-size:28px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;text-align:begin;line-height:100%;letter-spacing:0px;word-spacing:0px;writing-mode:lr-tb;text-anchor:begin;fill:#666666;fill-opacity:1;stroke:none;font-family:Bitstream Vera Sans', x='20', y='35')
  text.text=component['name'] 
  doc.append(text)

  if len(importlist)>0:
      # Add the import statements
  
      text=et.Element('text', style='font-size:16px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;text-align:begin;line-height:100%;letter-spacing:0px;word-spacing:0px;writing-mode:lr-tb;text-anchor:begin;fill:#666666;fill-opacity:1;stroke:none;font-family:Bitstream Vera Sans', x='20', y='70')
      text.text='@imports' 
      doc.append(text)
  
      text=et.Element('text', style='font-size:12px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;text-align:begin;line-height:100%;letter-spacing:0px;word-spacing:0px;writing-mode:lr-tb;text-anchor:begin;fill:#666666;fill-opacity:1;stroke:none;font-family:Bitstream Vera Sans', x='20', y='85')
      text.text=importlist[0] 
      if len(importlist)>1:
          for imports in importlist[1:]:
              text.text+=', ' + imports
      doc.append(text)

  offset = 100 + 12 * len(importlist)

  if len(exportlist)>0:
      # Add the export statements
      text=et.Element('text', style='font-size:16px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;text-align:begin;line-height:100%;letter-spacing:0px;word-spacing:0px;writing-mode:lr-tb;text-anchor:begin;fill:#666666;fill-opacity:1;stroke:none;font-family:Bitstream Vera Sans', x='20', y=str(offset))
      text.text='@exports' 
      doc.append(text)
  
      text=et.Element('text', style='font-size:12px;font-style:normal;font-variant:normal;font-weight:normal;font-stretch:normal;text-align:begin;line-height:100%;letter-spacing:0px;word-spacing:0px;writing-mode:lr-tb;text-anchor:begin;fill:#666666;fill-opacity:1;stroke:none;font-family:Bitstream Vera Sans', x='20', y=str(offset+15))
      text.text=''
      
      for i in range(0,len(exportlist)):
          item=top['exports'][exportlist[i]]
          text.text += exportlist[i]
          print item, item[0]
          if item is dict and item.has_key('units'):
              text.text += '[' + item['units'] + ']'
          if item is list and item.index('units')>-1:
              text.text += '[' + item.pop('units') + ']'
          if len(exportlist)>=i:
              text.text += ', '
      doc.append(text)

def write_document(filename,doc):
  f = open(filename, 'w')
  f.write('<?xml version=\"1.0\" standalone=\"no\"?>\n')
  f.write('<!DOCTYPE svg PUBLIC \"-//W3C//DTD SVG 1.1//EN\"\n')
  f.write('\"http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd\">\n')
  f.write(et.tostring(doc))
  f.close()

draw_component(doc,component)
write_document(component['name'] + '_component.svg',doc)

