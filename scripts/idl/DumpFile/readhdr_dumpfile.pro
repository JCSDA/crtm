;+
pro readhdr_dumpfile, fileid, hdr
;-
  hdr={dumpfile_hdr}
  record_length = hdr.record_length
  n_records     = hdr.n_records
  readu, fileid, record_length, n_records
  hdr.record_length = record_length
  hdr.n_records     = n_records
end
