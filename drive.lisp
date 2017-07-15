(defparameter exceptions
  '(directory-not-empty
    existing-attribute
    file-not-found
    io-error
    invalid-operation
    no-attribute
    permission-denied
    temporary-error))


(defvar folder-mime-type  "application/vnd.google-apps.folder")

(defvar file-fields
  '("appProperties" "capabilities(canEdit)" "createdTime,explicitlyTrashed"
    "fileExtension" "fullFileExtension" "id" "md5Checksum" "mimeType"
    "modifiedTime" "name" "parents" "size" "trashed" "version"
    "viewedByMeTime" "webViewLink"))
  
(defvar file-std-params)

;; Computed from file-fields  
(defvar file-list-std-params)



(defvar file-download-std-params)


;; Oauth2.do-request
(defun do-request ())


;; Thread.create
(defun async-do-request (f))


(defparameter root-directory "/")
(defparameter root-folder-id "root")
(defparameter trash-directory "/.Trash")
(defparameter trash-directory-name-length (length trash-directory))
(defparameter trash-directory-base-path "/.Trash/")
(defparameter lost-and-found-directory "/lost+found")
(defparameter shared-with-me-directory "/.shared")
(defparameter f-bsize 4096L)
(defparameter change-limit 50)
(defparameter max-link-target-length 127)
(defparameter max-attribute-length 126)

; Utilities
(defparameter chars-blacklist-regexp  "[/\000]")

;; replace all characters in name that occure in the black list with
;; something not on the black list, e.g. '-'.
(defun clean-filename (name)
  )

(defparameter apostrophe-regexp "'")

;; escape apostrophy from the name c.f. apostrophe-regexp
(defun escape-apostrophe (name))
  

(defun json-length (string-with-embedded-quotes)
  (let ((length-with-quotes (length s)))
	(- length-with-quotes 2)))

(defun get-remote-id-fingerprint (word-length remote-id)
  (when (> word-length 4)
    (error "Too many filename conflicts"))
  (let* (md5 md5_result hexa h offset)))

(defun disambiguate-filename (filename full-file-extension remote-id filename-table)
  (flet ((find-first-unique-filename (filename counter)
	   (let ((new-candidate 
		  (let ((fingerprint (get-remote-id-fingerprint counter remote-id)))
		    (cond ((string= full-file-extension "")
			   (printf:sprintf "%s (%s)" filename fingerprint))
			  ((string= full-file-extension extension)
			   (let ((base-name
				  (string:sub filename 0 (- (length filename) 
							    (- (length extension)- 1)))))
			     (printf:sprintf "%s (%s).%s" base-name fingerprint extension)))))))
	     (cond ((not (hashtbl:mem filename-table new-candidate))
		    (utils:log-with-header "Checking: %s: OK\n%!" new-candidate)
		    new-candidate)
		   (t

		    (utils:log-with-header "Checking: %s: KO\n%!" new-candidate)
		    (find-first-unique-filename filename (+ counter 1))))))))
  (cond ((hashtbl:mem filename-table filename then)
	 (utils:log-with-header "Filename collision detected: %s\n%!" filename)
	 (let ((unique-filename
		(find-first-unique-filename filename 1)))
	   (let ((name-counter (hashtbl:find filename-table filename)))
	     (hashtbl:replace filename-table filename (name-counter + 1))
	     unique-filename)))
	(t
	 (utils:log-with-header "Filename (unused): %s\n%!" filename)
	 (hashtbl:add filename-table filename 0)
	 filename)))

(defun is-in-trash-directory (path)
  (cond ((string= path trash-directory)
	 nil)
	(t (extstring-string:starts-with
	    path
	    trash-directory-base-path))))

(defun is-lost-and-found-root (path trashed config)
  (if (or trashed (not (lost-and-found (Config config))))
      nil
      lost-and-found-directory))

(defun is-lost-and-found (path trashed config)
  (if (or trashed (not (lost-and-found (Config config))))
      (ExtString-String:starts-with path lost-and-found-directory)))

(defun is-shared-with-me-root (trashed config)
  (if (or trashed (not (shared-with-me (Config config))))
      nil
      shared-with-me-directory))

(defun is-shared-with-me (trashed config)
  (if (or trashed (not (shared-with-me (Config config))))
      nil
      (extstring-string:starts-with path shared-with-me-directory)))

(defun get-path-in-cache (path)
  (cond
    ((string= path root-directory)
     (values root-directory false))
    ((string= path trash-directory)
     (values root-directory true))
    ((string= is-in-trash-directory path)
     (Str:string-after path trash-directory-name-length))
    (t
     (values path false))))

;; Resource cache 
(defun get-filename (name is-document get-document-format)
  (let* ((context (Context:get-ctx))
	 (config  (Context:config-lens context))
	 (clean-name  (clean-filename name))
	 (document-format (if is-document
			      (get-document-format config)
			      "")))
    (if (and is-document
	     (and (docs-file-extension (Config config))
		  (not (string= document-format ""))))
	(str  clean-name  "."  document-format)
	clean-name)))

(defun build-resource-tables (parent-path trashed)
  (let* ((context (context:get-ctx))
	 (cache  (cache (context context)))
	 (resources (cache-resource:select-resources-with-parent-path
		     cache
		     parent-path
		     trashed))
	 (filename-table  (hashtbl:create (hashtable-initial-size utils)))
	 (remote-id-table hashtbl:create (list:length resources))
	 (dolist (resource resources)

       (let* ((name (option:get (name (resource (cache resource)))))
	      (clean-name (get-filename name
					(is-document (resource cache))))
	      (my-lambda (lambda (config) (get-format (cache-resource:get-format resource config))))
	      (filename  (filename:basename (resource (cache (path resource))))))
	 (unless (not (string= clean-name filename))
	   (setf name-counter (hashtbl:find filename-table clean-name)))
         (hashtbl:replace filename-table clean-name name-counter)

       hashtbl.add filename-table filename 0;
       hashtbl.add remote-id-table
         (option.get resource.cache.resource.remote-id) resource)
    resources;
  (filename-table, remote-id-table)))))

let create-resource path =
  let parent-path = Filename.dirname path in
  { Cache.Resource.id = 0L;
    remote-id = None;
    name = None;
    mime-type = None;
    created-time = None;
    modified-time = None;
    viewed-by-me-time = None;
    file-extension = None;
    full-file-extension = None;
    md5-checksum = None;
    size = None;
    can-edit = None;
    trashed = None;
    web-view-link = None;
    version = None;
    file-mode-bits = None;
    uid = None;
    gid = None;
    link-target = None;
    xattrs = "";
    parent-path;
    path;
    state = Cache.Resource.State.ToDownload;
    last-update = Unix.gettimeofday ();
  }

(defun create-root-resource ()
  (let ((resource (create-resource root-directory)))
    (setf (remote-id (resource (cache resource))) (some root-folder-id))
        (setf (mime-type resource) (some folder-mime-type))
        (setf (size resource) (some 0l))
        (setf parent-path  "")
        (setf trashed  (some trashed))))

(defun create-well-known-resource (path)
  (let ((resource  (create-resource path)))
  (Cache-Resource:remote-id resource "")
        (setf (mime-type resource)  folder-mime-type)
        (setf (size  resource 0L))
        (setf (parent-path resource) "")
        (setf (trashed  resource) nil)))

(defun get-unique-filename (name full-file-extension remote-id is-document
			    get-document-format)
  (let ((complete-name (get-filename name is-document get-document-format)))
    (disambiguate-filename
     complete-name
     full-file-extension
     remote-id)))

(defun get-unique-filename-from-resource (resource name filename-table)
  (get-unique-filename name
		       (or resource.Cache.Resource.full-file-extension "")
		       (or resource.Cache.Resource.remote-id "")
		       (Cache.Resource.is-document resource)
		       (funcall (Cache-Resource:get-format resource config) config)
		       filename-table))

(defun get-unique-filename-from-file (file filename-table)
  get-unique-filename
    file.File.name
    file.File.fullFileExtension
    file.File.id
    (Cache.Resource.is-document-mime-type file.File.mimeType)
    (fun config ->
      Cache.Resource.get-format-from-mime-type file.File.mimeType config)
    filename-table

(defun recompute-path (resource name)
  (* TODO: make an optimized version of build-resource-tables that
   * doesn't create resource table (useful for large directories). *)
  let (filename-table, -) =
    build-resource-tables
      resource.Cache.Resource.parent-path
      (Option.default false resource.Cache.Resource.trashed) in
  let filename =
    get-unique-filename-from-resource resource name filename-table in
  Filename.concat resource.Cache.Resource.parent-path filename)

(defun update-resource-from-file (?state resource file)
  let path =
    match resource.Cache.Resource.name with
        Some cached-name ->
          if cached-name <> file.File.name then
            recompute-path resource file.File.name
          else resource.Cache.Resource.path
      | None -> resource.Cache.Resource.path
  in
  let parent-path = Filename.dirname path in
  { resource with
        Cache.Resource.remote-id = Some file.File.id;
        name = Some file.File.name;
        mime-type = Some file.File.mimeType;
        created-time = Some (Netdate.since-epoch file.File.createdTime);
        modified-time = Some (Netdate.since-epoch file.File.modifiedTime);
        viewed-by-me-time =
          Some (Netdate.since-epoch file.File.viewedByMeTime);
        file-extension = Some file.File.fileExtension;
        full-file-extension = Some file.File.fullFileExtension;
        md5-checksum = Some file.File.md5Checksum;
        size = Some file.File.size;
        can-edit = Some file.File.capabilities.File.Capabilities.canEdit;
        trashed = Some file.File.trashed;
        web-view-link = Some file.File.webViewLink;
        version = Some file.File.version;
        file-mode-bits = Cache.Resource.get-file-mode-bits
            file.File.appProperties;
        uid = Cache.Resource.get-uid file.File.appProperties;
        gid = Cache.Resource.get-gid file.File.appProperties;
        link-target = Cache.Resource.get-link-target file.File.appProperties;
        xattrs = Cache.Resource.get-xattrs file.File.appProperties;
        last-update = Unix.gettimeofday ();
        path;
        parent-path;
        state = Option.default resource.Cache.Resource.state state;
  })

(defun insert-resource-into-cache (?state cache resource file)
  let resource = update-resource-from-file ?state resource file in
  Utils.log-with-header "BEGIN: Saving resource to db (remote id=%s)\n%!"
    file.File.id;
  let inserted = Cache.Resource.insert-resource cache resource in
  Utils.log-with-header "END: Saving resource to db (remote id=%s, id=%Ld, state=%s)\n%!"
    file.File.id
    inserted.Cache.Resource.id
    (Cache.Resource.State.to-string inserted.Cache.Resource.state);
  inserted)

(defun update-cached-resource (cache resource)
  Utils.log-with-header
    "BEGIN: Updating resource in db (id=%Ld, state=%s)\n%!"
    resource.Cache.Resource.id
    (Cache.Resource.State.to-string resource.Cache.Resource.state);
  Cache.Resource.update-resource cache resource;
  Utils.log-with-header "END: Updating resource in db (id=%Ld)\n%!"
    resource.Cache.Resource.id)

(defun update-cached-resource-state (cache state id)
  Utils.log-with-header
    "BEGIN: Updating resource state in db (id=%Ld, state=%s)\n%!"
    id (Cache.Resource.State.to-string state);
  Cache.Resource.update-resource-state cache state id;
  Utils.log-with-header "END: Updating resource state in db (id=%Ld)\n%!" id)

(defun lookup-resource (path trashed)
  Utils.log-with-header "BEGIN: Loading resource %s (trashed=%b) from db\n%!"
    path trashed;
  let cache = Context.get-cache () in
  let resource = Cache.Resource.select-resource-with-path cache path trashed in
  begin if Option.is-none resource then begin
    Utils.log-with-header
      "END: Loading resource %s (trashed=%b) from db: Not found\n%!"
      path trashed;
  end else begin
    let id = resource |. GapiLens.option-get |. Cache.Resource.id in
    let state = resource
      |. GapiLens.option-get
      |. Cache.Resource.state
      |> Cache.Resource.State.to-string in
    Utils.log-with-header
      "END: Loading resource %s (trashed=%b) from db: Found (id=%Ld, state=%s)\n%!"
      path trashed id state;
  end end;
  resource)

(defun get-well-known-resource (path trashed)
  let context = Context.get-ctx () in
  let cache = context.Context.cache in
  let config = context |. Context.config-lens in
  match lookup-resource path trashed with
  | None ->
    let (well-known-resource, label) =
      if path = root-directory then
        (create-root-resource trashed, "root")
      else if is-lost-and-found-root path trashed config then
        (create-well-known-resource lost-and-found-directory, "lost+found")
      else if is-shared-with-me-root path trashed config then
        (create-well-known-resource shared-with-me-directory, "shared with me")
      else invalid-arg ("Invalid well known path: " ^ path ^ " trashed=" ^
                        (string-of-bool trashed))
    in
    Utils.log-with-header
      "BEGIN: Saving %s resource to db (id=%Ld)\n%!"
      label well-known-resource.Cache.Resource.id;
    let inserted =
      Cache.Resource.insert-resource cache well-known-resource in
    Utils.log-with-header
      "END: Saving %s resource to db (id=%Ld)\n%!"
      label well-known-resource.Cache.Resource.id;
    inserted
  | Some resource -> resource)

(defun update-cache-size (delta metadata cache)
  Utils.log-with-header "BEGIN: Updating cache size (delta=%Ld) in db\n%!"
    delta;
  if delta = 0L then begin
    Utils.log-with-header "END: No need to update cache size\n%!";
  end else begin
    Cache.Metadata.update-cache-size cache delta;
    let update-metadata context =
      let metadata = context.Context.metadata
                     |. GapiLens.option-get
                     |> Cache.Metadata.cache-size ^=
                        Int64.add metadata.Cache.Metadata.cache-size delta in
      Utils.log-with-header "END: Updating cache size (new size=%Ld) in db\n%!"
        metadata.Cache.Metadata.cache-size;
      context |> Context.metadata ^= Some metadata
    in
    Context.update-ctx update-metadata
  end)

(defun shrink-cache (?(file-size = 0L) ())
  let context = Context.get-ctx () in
  let metadata = context |. Context.metadata-lens in
  let config = context |. Context.config-lens in
  let max-cache-size-mb = config.Config.max-cache-size-mb in
  let cache = context.Context.cache in
  Utils.with-lock context.Context.metadata-lock
    (fun () ->
       let max-cache-size =
         Int64.mul (Int64.of-int max-cache-size-mb) Utils.mb in
       let target-size =
         Int64.add metadata.Cache.Metadata.cache-size file-size in
       if target-size > max-cache-size then begin
         let resources =
           Cache.Resource.select-resources-order-by-last-update cache in
         let (new-cache-size, total-delta, resources-to-free) =
           List.fold-left
             (fun (new-cache-size, delta, rs) resource ->
                if new-cache-size <= max-cache-size then
                  (new-cache-size, delta, rs)
                else begin
                  let size-to-free =
                    Option.default 0L resource.Cache.Resource.size in
                  let new-size = Int64.sub new-cache-size size-to-free in
                  let new-delta = Int64.add delta (Int64.neg size-to-free) in
                  (new-size, new-delta, resource :: rs)
                end)
             (target-size, file-size, [])
             resources in
         update-cache-size total-delta metadata cache;
         List.iter
           (fun resource ->
              update-cached-resource-state cache
                Cache.Resource.State.ToDownload resource.Cache.Resource.id)
           resources-to-free;
         Cache.delete-files-from-cache cache resources-to-free |> ignore
       end else begin
          update-cache-size file-size metadata cache;
       end))

(defun delete-memory-buffers (memory-buffers resource)
  Option.may
    (fun remote-id ->
       Buffering.MemoryBuffers.remove-buffers remote-id memory-buffers
    )
    resource.Cache.Resource.remote-id)

(defun delete-from-context (context resource)
  let memory-buffers = context.Context.memory-buffers in
  delete-memory-buffers memory-buffers resource;
  Option.may
    (fun remote-id ->
       Context.with-ctx-lock
         (fun () -> Hashtbl.remove context.Context.file-locks remote-id)
    )
    resource.Cache.Resource.remote-id)

(defun delete-cached-resource (resource)
  let context = Context.get-ctx () in
  let cache = context.Context.cache in
  Cache.Resource.delete-resource cache resource;
  let total-size =
    Cache.delete-files-from-cache cache [resource] in
  Option.may
    (fun metadata ->
       update-cache-size (Int64.neg total-size) metadata cache
    )
    context.Context.metadata;
  delete-from-context context resource)

(defun delete-cached-resources (metadata cache resources)
  Cache.Resource.delete-resources cache resources;
  let total-size =
    Cache.delete-files-from-cache cache resources in
  update-cache-size (Int64.neg total-size) metadata cache;
  let context = Context.get-ctx () in
  List.iter
    (delete-from-context context)
    resources)

(defun update-cache-size-for-documents (cache resource content-path op)
  let context = Context.get-ctx () in
  Utils.with-lock context.Context.metadata-lock
    (fun () ->
      if resource.Cache.Resource.size = Some 0L &&
          Sys.file-exists content-path then begin
        try
          let stats = Unix.LargeFile.stat content-path in
          let size = stats.Unix.LargeFile.st-size in
          let metadata = context |. Context.metadata-lens in
          let delta = op size in
          update-cache-size delta metadata cache
        with e -> Utils.log-exception e
      end)
(* END Resource cache *))

(defun let (get-metadata ())
  let request-new-start-page-token =
    let std-params =
      { GapiService.StandardParameters.default with
            GapiService.StandardParameters.fields =
              "startPageToken"
      } in
    ChangesResource.getStartPageToken
      ~std-params >>= fun startPageToken ->
    SessionM.return startPageToken.StartPageToken.startPageToken
  in)

(defun get-start-page-token (start-page-token-db)
    if start-page-token-db = "" then
      request-new-start-page-token
    else
      SessionM.return start-page-token-db
  in)

(defun request-metadata (start-page-token-db cache-size)
    let std-params =
      { GapiService.StandardParameters.default with
            GapiService.StandardParameters.fields =
              "user(displayName),storageQuota(limit,usage)"
      } in
    AboutResource.get
      ~std-params >>= fun about ->
    get-start-page-token start-page-token-db >>= fun start-page-token ->
    let metadata = {
      Cache.Metadata.display-name = about.About.user.User.displayName;
      storage-quota-limit = about.About.storageQuota.About.StorageQuota.limit;
      storage-quota-usage = about.About.storageQuota.About.StorageQuota.usage;
      start-page-token;
      cache-size;
      last-update = Unix.gettimeofday ();
    } in
    SessionM.return metadata
  in)

(defun context (= Context.get-ctx () )
  let cache = context.Context.cache in
  let config = context |. Context.config-lens in)

(defun update-resource-cache (new-metadata old-metadata)
    let get-all-changes =
      let rec loop pageToken accu =
        let std-params =
          { GapiService.StandardParameters.default with
                GapiService.StandardParameters.fields =
                  "changes(removed,file(parents,trashed),fileId),\
                   nextPageToken,newStartPageToken"
          } in
        ChangesResource.list
          ~std-params
          ~includeRemoved:true
          ~pageToken >>= fun change-list ->
        let changes = change-list.ChangeList.changes @ accu in
        if change-list.ChangeList.nextPageToken = "" then
          SessionM.return (changes, change-list.ChangeList.newStartPageToken)
        else
          loop change-list.ChangeList.nextPageToken changes
      in
      loop new-metadata.Cache.Metadata.start-page-token []
    in)

    let request-changes =
      Utils.log-with-header "BEGIN: Getting changes from server\n%!";
      get-all-changes >>= fun (changes, new-start-page-token) ->
      Utils.log-with-header "END: Getting changes from server\n%!";
      SessionM.return (changes, new-start-page-token)
    in

    let get-ids-to-update change =
      let get-id = Option.map (fun r -> r.Cache.Resource.id) in
      let selected-resource =
        Cache.Resource.select-resource-with-remote-id
          cache change.Change.fileId in
      let resources =
        match selected-resource with
            None ->
              let remote-ids =
                match change.Change.file.File.parents with
                    [] -> [root-folder-id]
                  | ids -> ids
              in
              List.map
                (Cache.Resource.select-resource-with-remote-id cache)
                remote-ids
          | Some r ->
              let parent-resource =
                Cache.Resource.select-resource-with-path cache
                  r.Cache.Resource.parent-path
                  false
              in
              [parent-resource; selected-resource]
      in
      List.map get-id resources
    in

    let get-file-id-from-change change =
      [Cache.Resource.select-resource-with-remote-id cache
         change.Change.fileId]
    in

    let request-remaining-changes start-page-token-db =
      if start-page-token-db = "" then
        SessionM.return (false, true)
      else
        let std-params =
          { GapiService.StandardParameters.default with
                GapiService.StandardParameters.fields = "newStartPageToken"
          } in
        ChangesResource.list
          ~std-params
          ~includeRemoved:true
          ~pageSize:change-limit
          ~pageToken:start-page-token-db >>= fun change-list ->
        let (no-changes, over-limit) =
          (change-list.ChangeList.newStartPageToken = start-page-token-db,
           change-list.ChangeList.newStartPageToken = "") in
        SessionM.return (no-changes, over-limit)
    in

    request-remaining-changes
      new-metadata.Cache.Metadata.start-page-token >>= fun (no-changes,
                                                            over-limit) ->
    if no-changes then begin
      Utils.log-with-header
        "END: Getting metadata: No need to update resource cache\n%!";
      Utils.log-with-header "BEGIN: Updating timestamps\n%!";
      Cache.Resource.update-all-timestamps cache
        new-metadata.Cache.Metadata.last-update;
      Utils.log-with-header "END: Updating timestamps\n%!";
      SessionM.return new-metadata
    end else if over-limit then begin
      Utils.log-with-header "END: Getting metadata: Too many changes\n";
      Utils.log-with-header "BEGIN: Getting new start page token\n%!";
      get-start-page-token "" >>= fun new-start-page-token ->
      Utils.log-with-header "END: Getting new start page token (%s)\n%!"
        new-start-page-token;
      Utils.log-with-header "BEGIN: Invalidating resources\n%!";
      Cache.Resource.invalidate-all cache;
      Utils.log-with-header "END: Invalidating resources\n%!";
      SessionM.return {
        new-metadata with
            Cache.Metadata.start-page-token = new-start-page-token;
      }
    end else begin
      Utils.log-with-header "BEGIN: Updating timestamps\n%!";
      Cache.Resource.update-all-timestamps cache
        new-metadata.Cache.Metadata.last-update;
      Utils.log-with-header "END: Updating timestamps\n%!";
      match old-metadata with
        None ->
          SessionM.return new-metadata
      | Some - ->
          request-changes >>= fun (changes, new-start-page-token) ->
          let update-resource-cache-from-changes
              filter-changes map-change update-cache =
            let filtered-changes =
              List.filter filter-changes changes in
            let xs =
              List.fold-left
                (fun xs change ->
                   let mapped-changes = map-change change in
                     List.fold-left
                       (fun xs' c ->
                          match c with
                              None -> xs'
                            | Some x ->
                                if not (List.mem x xs') then
                                  x :: xs'
                                else xs')
                       xs
                       mapped-changes)
                []
                filtered-changes in
            update-cache cache xs;
          in

          Utils.log-with-header "BEGIN: Updating resource cache\n%!";
          update-resource-cache-from-changes
            (fun change ->
               not change.Change.removed &&
               not change.Change.file.File.trashed)
            get-ids-to-update
            (fun cache ids ->
               Cache.Resource.invalidate-resources cache ids);
          Utils.log-with-header "END: Updating resource cache\n";
          Utils.log-with-header "BEGIN: Updating trashed resources\n%!";
          update-resource-cache-from-changes
            (fun change -> change.Change.file.File.trashed)
            get-file-id-from-change
            (fun cache resources ->
               Cache.Resource.trash-resources cache resources);
          Utils.log-with-header "END: Updating trashed resources\n";
          Utils.log-with-header "BEGIN: Removing deleted resources\n%!";
          update-resource-cache-from-changes
            (fun change -> change.Change.removed)
            get-file-id-from-change
            (delete-cached-resources new-metadata);
          Utils.log-with-header "END: Removing deleted resources\n%!";
          if List.length changes > 0 then begin
            Utils.log-with-header
              "BEGIN: Invalidating trash bin resource\n%!";
            Cache.Resource.invalidate-trash-bin cache;
            Utils.log-with-header "END: Invalidating trash bin resource\n%!";
            if config.Config.lost-and-found then begin
              Utils.log-with-header
                "BEGIN: Invalidating lost+found resource\n%!";
              Cache.Resource.invalidate-path cache lost-and-found-directory;
              Utils.log-with-header
                "END: Invalidating lost+found resource\n%!";
            end;
            if config.Config.shared-with-me then begin
              Utils.log-with-header
                "BEGIN: Invalidating .shared resource\n%!";
              Cache.Resource.invalidate-path cache shared-with-me-directory;
              Utils.log-with-header
                "END: Invalidating .shared resource\n%!";
            end
          end;
          SessionM.return {
            new-metadata with
                Cache.Metadata.start-page-token = new-start-page-token;
          }
    end
  in

  let refresh-metadata old-metadata =
    let start-page-token =
      Option.map-default
        Cache.Metadata.start-page-token.GapiLens.get "" old-metadata in
    let cache-size =
      Option.map-default
        Cache.Metadata.cache-size.GapiLens.get 0L old-metadata in
    Utils.log-with-header "BEGIN: Refreshing metadata\n%!";
    try-with-default
      (request-metadata start-page-token cache-size) >>= fun server-metadata ->
    Utils.log-with-header "END: Refreshing metadata\n";
    update-resource-cache
      server-metadata old-metadata >>= fun updated-metadata ->
    Utils.log-with-header "BEGIN: Updating metadata in db\n%!";
    Cache.Metadata.insert-metadata context.Context.cache updated-metadata;
    Utils.log-with-header "END: Updating metadata in db\n";
    Utils.log-with-header "BEGIN: Updating context\n%!";
    Context.update-ctx (Context.metadata ^= Some updated-metadata);
    Utils.log-with-header "END: Updating context\n%!";
    SessionM.return updated-metadata
  in

  let resync-cache-size db-metadata =
    let old-cache-size = db-metadata.Cache.Metadata.cache-size in
    Utils.log-with-header
      "BEGIN: Recalculating cache size (old value=%Ld)\n%!"
      old-cache-size;
    let cache-size = Cache.compute-cache-size context.Context.cache in
    Utils.log-with-header
      "END: Recalculating cache size (new value=%Ld)\n%!"
      cache-size;
    db-metadata |> Cache.Metadata.cache-size ^= cache-size
  in

  Utils.with-lock context.Context.metadata-lock
    (fun () ->
       let metadata =
         let context = Context.get-ctx () in
         if Option.is-none context.Context.metadata then begin
           Utils.log-with-header "BEGIN: Loading metadata from db\n%!";
           let db-metadata =
             Cache.Metadata.select-metadata context.Context.cache in
           let db-metadata =
             Option.map resync-cache-size db-metadata in
           Context.update-ctx (Context.metadata ^= db-metadata);
           db-metadata
         end else begin
           Utils.log-with-header "BEGIN: Getting metadata from context\n%!";
           context.Context.metadata
         end in

       match metadata with
         None ->
           Utils.log-with-header "END: Getting metadata: Not found\n%!";
           do-request (refresh-metadata metadata) |> fst
       | Some m ->
           let metadata-cache-time =
             context |. Context.config-lens |. Config.metadata-cache-time
           in
           if Cache.Metadata.is-valid metadata-cache-time m then begin
             Utils.log-with-header "END: Getting metadata: Valid\n%!";
             m
           end else begin
             Utils.log-with-header "END: Getting metadata: Not valid\n%!";
             do-request (refresh-metadata metadata) |> fst
           end
    )

(defun statfs (())
  let metadata = get-metadata () in
  let limit =
    if metadata.Cache.Metadata.storage-quota-limit = 0L then Int64.max-int
    else metadata.Cache.Metadata.storage-quota-limit in
  let f-blocks = Int64.div limit f-bsize in
  let free-bytes =
    Int64.sub limit metadata.Cache.Metadata.storage-quota-usage in
  let f-bfree = Int64.div free-bytes f-bsize in
  { Unix-util.f-bsize;
    f-blocks;
    f-bfree;
    f-bavail = f-bfree;
    f-files = f-blocks;
    f-ffree = f-bfree;
    f-namemax = 256L;
    (* ignored *)
    f-frsize = 0L;
    f-favail = 0L;
    f-fsid = 0L;
    f-flag = 0L;
  }
(* END Metadata *))

(defun let (get-file-from-server parent-folder-id name trashed)
  Utils.log-with-header
    "BEGIN: Getting resource %s (in folder %s) from server\n%!"
    name parent-folder-id;
  let q =
    Printf.sprintf "name='%s' and '%s' in parents and trashed=%b"
      (escape-apostrophe name) parent-folder-id trashed in
  FilesResource.list
    ~std-params:file-list-std-params
    ~q
    ~pageSize:1 >>= fun file-list ->
  Utils.log-with-header
    "END: Getting resource %s (in folder %s) from server\n%!"
    name parent-folder-id;
  let files = file-list.FileList.files in
  if List.length files = 0 then
    SessionM.return None
  else
    let file = files |. GapiLens.head in
    SessionM.return (Some file))

(defun get-resource-from-server (parent-folder-id name new-resource trashed cache)
  get-file-from-server parent-folder-id name trashed >>= fun file ->
  match file with
      None ->
        Utils.log-with-header
          "BEGIN: Saving not found resource to db (name=%s)\n%!"
          name;
        let resource = new-resource
          |> Cache.Resource.trashed ^= Some trashed
          |> Cache.Resource.state ^= Cache.Resource.State.NotFound in
        let inserted = Cache.Resource.insert-resource cache resource in
        Utils.log-with-header
          "END: Saving not found resource to db (name=%s)\n%!"
          name;
        SessionM.return inserted
    | Some entry ->
        let inserted = insert-resource-into-cache cache new-resource entry in
        SessionM.return inserted)

(defun check-resource-in-cache (cache path trashed)
  let metadata-last-update =
    Context.get-ctx () |. Context.metadata-last-update-lens in
  match lookup-resource path trashed with
      None -> false
    | Some resource ->
        if Cache.Resource.is-valid resource metadata-last-update then
          if Cache.Resource.is-folder resource then
            resource.Cache.Resource.state = Cache.Resource.State.Synchronized
          else true
        else false)

(defun rec (get-folder-id path trashed)
  if path = root-directory then
    SessionM.return root-folder-id
  else
    get-resource path trashed >>= fun resource ->
    let remote-id =
      resource |. Cache.Resource.remote-id |. GapiLens.option-get in
    SessionM.return remote-id
and get-resource path trashed =
  let config = Context.get-ctx () |. Context.config-lens in
  let metadata-last-update =
    get-metadata () |. Cache.Metadata.last-update in)

  let get-new-resource cache =
    let parent-path = Filename.dirname path in
      if check-resource-in-cache cache parent-path trashed then begin
        (* If parent-path is up to date, all resources are already cached,
         * so a new resource must be a "not found" one. *)
        Utils.raise-m File-not-found
      end else begin
        let new-resource = create-resource path in
        let name = Filename.basename path in
        get-folder-id
          new-resource.Cache.Resource.parent-path
          trashed >>= fun parent-folder-id ->
        get-resource-from-server
          parent-folder-id name new-resource trashed cache >>= fun resource ->
        SessionM.return resource
      end
  in

  let refresh-resource resource cache =
    begin if Option.is-some resource.Cache.Resource.remote-id then begin
      let remote-id = resource.Cache.Resource.remote-id |> Option.get in
      Utils.log-with-header
        "BEGIN: Getting file from server (remote id=%s)\n%!"
        remote-id;
      FilesResource.get
        ~std-params:file-std-params
        ~fileId:remote-id >>= fun file ->
      Utils.log-with-header
        "END: Getting file from server (remote id=%s)\n%!"
        remote-id;
      SessionM.return (Some file)
    end else
      SessionM.return None
    end >>= fun refreshed-file ->
    match refreshed-file with
        None ->
          delete-cached-resource resource;
          get-new-resource cache
      | Some file ->
          let reloaded-resource = Option.map-default
              (Cache.Resource.select-resource-with-remote-id cache)
              (Some resource)
              resource.Cache.Resource.remote-id |> Option.default resource in
          let updated-resource = update-resource-from-file
              reloaded-resource file in
          update-cached-resource cache updated-resource;
          Utils.log-with-header
            "END: Refreshing resource (id=%Ld)\n%!"
            updated-resource.Cache.Resource.id;
          SessionM.return updated-resource
  in

  if path = root-directory then
    let root-resource =
      get-well-known-resource root-directory trashed in
    SessionM.return root-resource
  else if is-lost-and-found-root path trashed config then
    let lost-and-found-resource =
      get-well-known-resource lost-and-found-directory trashed in
    SessionM.return lost-and-found-resource
  else if is-shared-with-me-root path trashed config then
    let shared-with-me-resource =
      get-well-known-resource shared-with-me-directory trashed in
    SessionM.return shared-with-me-resource
  else
    let cache = Context.get-cache () in
    begin match lookup-resource path trashed with
        None ->
          get-new-resource cache
      | Some resource ->
          if Cache.Resource.is-valid resource metadata-last-update then
            SessionM.return resource
          else
            try-with-default (refresh-resource resource cache)
    end >>= fun resource ->
    begin match resource.Cache.Resource.state with
        Cache.Resource.State.NotFound ->
          Utils.raise-m File-not-found
      | - ->
          SessionM.return resource
    end

let check-md5-checksum resource cache =
  let path = resource.Cache.Resource.path in
  let content-path = Cache.get-content-path cache resource in
  let md5-checksum = Option.default "" resource.Cache.Resource.md5-checksum in
  if md5-checksum <> "" then begin
    Utils.log-with-header
      "BEGIN: Checking MD5 checksum (path=%s, cache path=%s, hash=%s)\n%!"
      path content-path md5-checksum;
    if Sys.file-exists content-path then begin
      let md5 = Cryptokit.Hash.md5 () in
      Utils.with-in-channel content-path
        (fun ch ->
           try
             while true do
               let byte = input-byte ch in
                 md5#add-byte byte;
             done
           with End-of-file -> ());
      let md5-result = md5#result in
      let hexa = Cryptokit.Hexa.encode () in
      hexa#put-string md5-result;
      hexa#finish;
      let checksum = hexa#get-string in
      Utils.log-with-header
        "END: Checking MD5 checksum (path=%s, cache path=%s, hash=%s): Computed MD5 checksum: %s\n%!"
        path content-path md5-checksum checksum;
      checksum = md5-checksum
    end else begin
      Utils.log-with-header
        "END: Checking MD5 checksum (path=%s, cache path=%s, hash=%s): File does not exists\n%!"
        path content-path md5-checksum;
      false
    end;
  end else false)

(defun with-retry (f resource)
  let rec loop res n =
    Utils.try-with-m
      (f res)
      (function
           Temporary-error ->
             if n >= !Utils.max-retries then begin
               Utils.raise-m IO-error
             end else begin
               GapiUtils.wait-exponential-backoff n;
               let fileId = res.Cache.Resource.remote-id |> Option.get in
               FilesResource.get
                 ~std-params:file-std-params
                 ~fileId >>= fun file ->
               let (state, verb) =
                 if resource.Cache.Resource.state =
                    Cache.Resource.State.ToUpload then
                   (Cache.Resource.State.ToUpload, "uploading")
                 else
                   (Cache.Resource.State.ToDownload, "downloading") in
               let refreshed-resource =
                 update-resource-from-file
                   ~state res file in
               let context = Context.get-ctx () in
               let cache = context.Context.cache in
               update-cached-resource cache refreshed-resource;
               let n' = n + 1 in
               Utils.log-with-header
                 "Retry (%d/%d) %s resource (id=%Ld).\n%!"
                 n' !Utils.max-retries verb resource.Cache.Resource.id;
               loop refreshed-resource n'
             end
         | e -> Utils.raise-m e)
  in
    loop resource 0)

(defun is-desktop-format (resource config)
  Cache.Resource.get-format resource config = "desktop")

(defun create-desktop-entry (resource content-path config)
  Utils.with-out-channel
    ~mode:[Open-creat; Open-trunc; Open-wronly] content-path
    (fun out-ch ->
      let icon-entry =
        let icon = Cache.Resource.get-icon resource config in
        if icon = "" then ""
        else "Icon=" ^ icon ^ "\n"
      in
      Printf.fprintf out-ch
        "[Desktop Entry]\n\
         Type=Link\n\
         Name=%s\n\
         URL=%s\n%s"
        (Option.default "" resource.Cache.Resource.name)
        (Option.default "" resource.Cache.Resource.web-view-link)
        icon-entry))

(defun download-resource (resource)
  let context = Context.get-ctx () in
  let cache = context.Context.cache in
  let config = context |. Context.config-lens in
  let content-path = Cache.get-content-path cache resource in
  let shrink-cache-before-downloading () =
    let file-size = Option.default 0L resource.Cache.Resource.size in
    shrink-cache ~file-size ();
    SessionM.return () in
  let do-api-download () =
    let destination = GapiMediaResource.TargetFile content-path in
    let media-download = {
      GapiMediaResource.destination;
      range-spec = "";
    } in
    let fileId = resource.Cache.Resource.remote-id |> Option.get in
    if Cache.Resource.is-document resource then begin
      let fmt = Cache.Resource.get-format resource config in
      let mimeType = Cache.Resource.mime-type-of-format fmt in
      FilesResource.export
        ~media-download
        ~fileId
        ~mimeType >>= fun () ->
      SessionM.return ()
    end else if Option.default 0L resource.Cache.Resource.size > 0L then begin
      FilesResource.get
        ~std-params:file-download-std-params
        ~media-download
        ~fileId >>= fun - ->
      SessionM.return ()
    end else begin
      Utils.log-with-header
        "BEGIN: Creating resource without content (path=%s)\n%!"
        content-path;
      close-out (open-out content-path);
      SessionM.return ()
    end
  in
  let do-download =
    SessionM.return () >>= fun () ->
    Utils.log-with-header
      "BEGIN: Downloading resource (id=%Ld) to %s\n%!"
      resource.Cache.Resource.id content-path;
    begin if is-desktop-format resource config then begin
      shrink-cache-before-downloading () >>= fun () ->
      update-cache-size-for-documents cache resource content-path Int64.neg;
      create-desktop-entry resource content-path config;
      SessionM.return ()
    end else begin
      shrink-cache-before-downloading () >>= fun () ->
      update-cached-resource-state cache
        Cache.Resource.State.Downloading resource.Cache.Resource.id;
      update-cache-size-for-documents cache resource content-path Int64.neg;
      Utils.try-with-m
        (do-api-download ())
        (fun e ->
           update-cached-resource-state cache
             Cache.Resource.State.ToDownload resource.Cache.Resource.id;
           handle-default-exceptions e)
    end end >>= fun () ->
    update-cache-size-for-documents cache resource content-path Std.identity;
    Utils.log-with-header
      "END: Downloading resource (id=%Ld) to %s\n%!"
      resource.Cache.Resource.id content-path;
    update-cached-resource-state cache
      Cache.Resource.State.Synchronized resource.Cache.Resource.id;
    SessionM.return ()
  in
  let get-lock () =
    let context = Context.get-ctx () in
    Context.with-ctx-lock
      (fun () ->
         let remote-id = resource.Cache.Resource.remote-id |> Option.get in
         match Utils.safe-find context.Context.file-locks remote-id with
         | None ->
           let mutex = Mutex.create () in
           Hashtbl.add context.Context.file-locks remote-id mutex;
           mutex
         | Some mutex -> mutex
      )
  in
  let do-download-with-lock () =
    let mutex = get-lock () in
    Utils.with-lock-m mutex do-download
  in
  let rec check-state n =
    let reloaded-resource = Option.map-default
      (Cache.Resource.select-resource-with-remote-id cache)
      (Some resource)
      resource.Cache.Resource.remote-id
    in
    let reloaded-state = match reloaded-resource with
        None -> Cache.Resource.State.NotFound
      | Some r -> r.Cache.Resource.state
    in
    let download-if-not-updated () =
      let r = reloaded-resource |> Option.get in
      if check-md5-checksum r cache then begin
        update-cached-resource-state cache
          Cache.Resource.State.Synchronized resource.Cache.Resource.id;
        SessionM.return ()
      end else
        do-download-with-lock ()
    in
    begin match reloaded-state with
        Cache.Resource.State.Synchronized
      | Cache.Resource.State.ToUpload
      | Cache.Resource.State.Uploading ->
          if Sys.file-exists content-path then
            SessionM.return ()
          else
            do-download-with-lock ()
      | Cache.Resource.State.ToDownload ->
          download-if-not-updated ()
      | Cache.Resource.State.Downloading ->
          if n > 300 then begin
            Utils.log-with-header
              "Still downloading resource (id=%Ld) after about 5 hours: start downloading again\n%!"
              resource.Cache.Resource.id;
            download-if-not-updated ()
          end else begin
            Utils.log-with-header
              "Already downloading resource (id=%Ld): check number %d\n%!"
              resource.Cache.Resource.id
              n;
            let n' = min n 6 in
            GapiUtils.wait-exponential-backoff n';
            check-state (n + 1)
          end
      | Cache.Resource.State.NotFound ->
          Utils.raise-m File-not-found
    end
  in
  check-state 0 >>= fun () ->
  SessionM.return content-path)

(defun stream-resource (offset buffer resource)
  let length = Bigarray.Array1.dim buffer in
  let finish = Int64.add offset (Int64.of-int (length - 1)) in
  Utils.log-with-header
    "BEGIN: Stream resource (id=%Ld, offset=%Ld, finish=%Ld, length=%d)\n%!"
    resource.Cache.Resource.id offset finish length;
  let destination = GapiMediaResource.ArrayBuffer buffer in
  let range-spec =
    GapiMediaResource.generate-range-spec [(Some offset, Some finish)] in
  let media-download = {
    GapiMediaResource.destination;
    range-spec;
  } in
  let fileId = resource |. Cache.Resource.remote-id |> Option.get in
  try-with-default
    (FilesResource.get
       ~std-params:file-download-std-params
       ~media-download
       ~fileId
    ) >>= fun - ->
  Utils.log-with-header
    "END: Stream resource (id=%Ld, offset=%Ld, finish=%Ld, length=%d)\n%!"
    resource.Cache.Resource.id offset finish length;
  SessionM.return ())

(defun stream-resource-to-memory-buffer (offset buffer resource)
  let context = Context.get-ctx () in
  let memory-buffers = context.Context.memory-buffers in
  let remote-id = resource.Cache.Resource.remote-id |> Option.get in
  Buffering.MemoryBuffers.read-block
    remote-id offset (resource.Cache.Resource.size |> Option.get)
    (fun start-pos block-buffer ->
       stream-resource start-pos block-buffer resource)
    ~dest-arr:buffer memory-buffers >>= fun () ->
  SessionM.return ())

(defun stream-resource-to-read-ahead-buffers (offset resource)
  let context = Context.get-ctx () in
  let memory-buffers = context.Context.memory-buffers in
  let remote-id = resource.Cache.Resource.remote-id |> Option.get in
  let config = context |. Context.config-lens in
  Buffering.MemoryBuffers.read-ahead config.Config.read-ahead-buffers
    remote-id offset (resource.Cache.Resource.size |> Option.get)
    (fun start-pos block-buffer ->
       stream-resource start-pos block-buffer resource)
    memory-buffers >>= fun ms ->
  List.map
    (fun m -> with-retry (fun - -> m) resource)
    ms |> SessionM.return)

(defun is-filesystem-read-only (())
  Context.get-ctx () |. Context.config-lens |. Config.read-only)

(defun is-file-read-only (resource)
  let config = Context.get-ctx () |. Context.config-lens in
  not (Option.default true resource.Cache.Resource.can-edit) ||
  Cache.Resource.is-document resource ||
  config.Config.large-file-read-only &&
    Cache.Resource.is-large-file config resource)

(defun let (get-attr path)
  let context = Context.get-ctx () in
  let config = context |. Context.config-lens in
  let (path-in-cache, trashed) = get-path-in-cache path in)

  let request-resource =
    get-resource path-in-cache trashed >>= fun resource ->
    begin if Cache.Resource.is-document resource &&
             config.Config.download-docs then
      Utils.try-with-m
        (with-retry download-resource resource)
        (function
             File-not-found -> SessionM.return ""
           | e -> Utils.raise-m e)
    else
      SessionM.return ""
    end >>= fun content-path ->
    SessionM.return (resource, content-path)
  in

  if path = root-directory then
    context.Context.mountpoint-stats
  else if path = trash-directory ||
          is-shared-with-me-root path trashed config then
    let stats = context.Context.mountpoint-stats in
    { stats with
      Unix.LargeFile.st-perm = stats.Unix.LargeFile.st-perm land 0o555
    }
  else if is-lost-and-found-root path trashed config then
    context.Context.mountpoint-stats
  else begin
    let (resource, content-path) = do-request request-resource |> fst in
    let stat =
      if content-path <> "" then Some (Unix.LargeFile.stat content-path)
      else None in
    let st-kind =
      if Cache.Resource.is-folder resource then Unix.S-DIR
      else
        Option.map-default
          Cache.Resource.file-mode-bits-to-kind
          Unix.S-REG
          resource.Cache.Resource.file-mode-bits
    in
    let st-perm =
      let default-perm =
        if Cache.Resource.is-folder resource then 0o777
        else 0o666 in
      let perm =
        Option.map-default
          Cache.Resource.file-mode-bits-to-perm
          default-perm
          resource.Cache.Resource.file-mode-bits
      in
      let mask =
        if Cache.Resource.is-symlink resource then 0o777
        else
          lnot config.Config.umask land (
            if is-file-read-only resource
            then 0o555
            else 0o777)
      in
      perm land mask in
    let st-nlink =
      if Cache.Resource.is-folder resource then 2
      else 1 in
    let st-uid =
      Option.map-default 
        Int64.to-int
        context.Context.mountpoint-stats.Unix.LargeFile.st-uid
        resource.Cache.Resource.uid in
    let st-gid =
      Option.map-default 
        Int64.to-int
        context.Context.mountpoint-stats.Unix.LargeFile.st-gid
        resource.Cache.Resource.gid in
    let st-size =
      if Cache.Resource.is-symlink resource then
        resource.Cache.Resource.link-target
          |> Option.get
          |> String.length
          |> Int64.of-int
      else match stat with
          None ->
            if Cache.Resource.is-folder resource then f-bsize
            else Option.default 0L resource.Cache.Resource.size
        | Some st ->
            st.Unix.LargeFile.st-size in
    let st-atime =
      match stat with
          None ->
            resource.Cache.Resource.viewed-by-me-time |> Option.get
        | Some st ->
            st.Unix.LargeFile.st-atime in
    let is-to-upload =
      resource.Cache.Resource.state = Cache.Resource.State.ToUpload in
    let st-mtime =
      match stat with
          Some st when is-to-upload ->
            st.Unix.LargeFile.st-mtime
        | - ->
            resource.Cache.Resource.modified-time |> Option.get in
    let st-ctime =
      match stat with
          Some st when is-to-upload ->
            st.Unix.LargeFile.st-ctime
        | - ->
            st-mtime
    in
    { context.Context.mountpoint-stats with
          Unix.LargeFile.st-kind;
          st-perm;
          st-nlink;
          st-uid;
          st-gid;
          st-size;
          st-atime;
          st-mtime;
          st-ctime;
    }
  end)

(defun read-dir (path)
  let get-all-files q =
    let rec loop ?pageToken accu =
      FilesResource.list
        ~std-params:file-list-std-params
        ~q
        ?pageToken >>= fun file-list ->
      let files = file-list.FileList.files @ accu in
      if file-list.FileList.nextPageToken = "" then
        SessionM.return files
      else
        loop ~pageToken:file-list.FileList.nextPageToken files
    in
      loop []
  in)

  let (path-in-cache, trashed) = get-path-in-cache path in
  let context = Context.get-ctx () in
  let cache = context.Context.cache in
  let config = context |. Context.config-lens in

  let request-folder =
    Utils.log-with-header
      "BEGIN: Getting folder content (path=%s, trashed=%b)\n%!"
      path-in-cache trashed;
    get-resource path-in-cache trashed >>= fun resource ->
    if is-lost-and-found-root path trashed config then begin
      Utils.log-with-header "BEGiN: Getting lost and found files\n%!";
      let q = "'me' in owners" in
      get-all-files q >>= fun all-owned-files ->
      let lost-and-found-files =
        List.filter
          (fun file -> file.File.parents = [])
          all-owned-files in
      Utils.log-with-header
        "END: Getting lost and found files: Found %d files\n%!"
        (List.length lost-and-found-files);
      SessionM.return (lost-and-found-files, resource);
    end else if is-shared-with-me-root path trashed config then begin
      Utils.log-with-header "BEGiN: Getting shared with me files\n%!";
      let q = "sharedWithMe = true" in
      get-all-files q >>= fun shared-with-me-files ->
      Utils.log-with-header
        "END: Getting shared with me files: Found %d files\n%!"
        (List.length shared-with-me-files);
      SessionM.return (shared-with-me-files, resource);
    end else begin
      get-folder-id path-in-cache trashed >>= fun folder-id ->
      let q =
        Printf.sprintf "'%s' in parents and trashed = %b" folder-id trashed in
      get-all-files q >>= fun files ->
      Utils.log-with-header
        "END: Getting folder content (path=%s, trashed=%b)\n%!"
        path-in-cache trashed;
      begin if path = trash-directory && trashed then begin
        Utils.log-with-header "BEGiN: Getting explicitly trashed files\n%!";
        let q =
          Printf.sprintf "not '%s' in parents and trashed = true" folder-id in
        get-all-files q >>= fun trashed-files ->
        let explicitly-trashed-files =
          List.filter (fun file -> file.File.explicitlyTrashed) trashed-files in
        Utils.log-with-header
          "END: Getting explicitly trashed files: Found %d files\n%!"
          (List.length explicitly-trashed-files);
        SessionM.return (files @ explicitly-trashed-files, resource);
      end else
        SessionM.return (files, resource);
      end
    end
  in

  let resources =
    if check-resource-in-cache cache path-in-cache trashed then begin
      Utils.log-with-header
        "BEGIN: Getting resources from db (parent path=%s, trashed=%b)\n%!"
        path-in-cache trashed;
      let resources =
        Cache.Resource.select-resources-with-parent-path
          cache path-in-cache trashed in
      Utils.log-with-header
        "END: Getting resources from db (parent path=%s, trashed=%b)\n%!"
        path-in-cache trashed;
      resources
    end else begin
      let (files, folder-resource) = do-request request-folder |> fst in
      let (filename-table, remote-id-table) =
        build-resource-tables path-in-cache trashed in
      let resources-and-files =
        List.map
          (fun file ->
             try
               let cached-resource =
                 Hashtbl.find remote-id-table file.File.id in
               let updated-resource =
                 update-resource-from-file cached-resource file in
               (Some updated-resource, file)
             with Not-found ->
               (None, file))
          files
      in
      let resources =
        List.map
          (fun (resource, file) ->
             match resource with
                 Some r -> r
               | None ->
                   let filename =
                     get-unique-filename-from-file file filename-table in
                   let resource-path =
                     Filename.concat path-in-cache filename in
                   let resource = create-resource resource-path in
                   update-resource-from-file resource file)
          resources-and-files
      in

      Utils.log-with-header
        "BEGIN: Inserting folder resources into db (trashed=%b)\n%!"
        trashed;
      let inserted-resources =
        Cache.Resource.insert-resources
          cache resources path-in-cache trashed in
      Utils.log-with-header
        "END: Inserting folder resources into db (trashed=%b)\n%!"
        trashed;
      let updated-resource = folder-resource
        |> Cache.Resource.state ^= Cache.Resource.State.Synchronized
        |> Cache.Resource.last-update ^= Unix.gettimeofday ()
      in
      update-cached-resource cache updated-resource;
      inserted-resources
    end
  in
  let filenames =
    List.map
      (fun resource ->
         Filename.basename resource.Cache.Resource.path)
      resources in
  let filenames =
    if path = root-directory then
      (Filename.basename trash-directory) :: filenames
    else filenames in
  let filenames =
    if path = root-directory && not trashed &&
       config.Config.shared-with-me then
      (Filename.basename shared-with-me-directory) :: filenames
    else filenames in
  if path = root-directory && not trashed &&
     config.Config.lost-and-found then
    (Filename.basename lost-and-found-directory) :: filenames
  else filenames)


(defun fopen (path flags)
  (multiple-value-bind (path-in-cache trashed) (get-path-in-cache path)
    (let* ((is-read-only-request (list:mem unix.o-rdonly flags))
	   (check-editable
	    (progn
	      (get-resource path-in-cache trashed)))
      (when (and (not is-read-only-request) (is-file-read-only resource))
	raise permission-denied)
      (when (and (not is-read-only-request) is-filesystem-read-only)
	  (raise permission-denied)
  else begin
    do-request check-editable |> ignore
  end;
  None)

(defun opendir (path flags)
  (multiple-value-bind (path-in-cache trashed)
      (get-path-in-cache path)
    (do-request :get-resource path-in-cache trashed)))

(defun default-save-resource-to-db (cache resource file)
  let updated-resource = update-resource-from-file resource file in
  update-cached-resource cache updated-resource)

(defun update-remote-resource (pa)
      ?update-file-in-cache
      ?(save-to-db = default-save-resource-to-db)
      ?(purge-cache = fun cache resource -> ())
      do-remote-update
      retry-update =
  let context = Context.get-ctx () in
  let cache = context.Context.cache in
  let (path-in-cache, trashed) = get-path-in-cache path in
  let update-file =
    get-resource path-in-cache trashed >>= fun resource ->
    try-with-default (do-remote-update resource) >>= fun file-option ->
    begin match file-option with
        None ->
          purge-cache cache resource
      | Some file ->
          begin match update-file-in-cache with
              None -> ()
            | Some go ->
                if resource.Cache.Resource.state =
                   Cache.Resource.State.Synchronized
                then begin
                  let content-path = Cache.get-content-path cache resource in
                  if Sys.file-exists content-path then
                    go content-path
                end;
          end;
          save-to-db cache resource file
    end;
    SessionM.return ()
  in
  if is-filesystem-read-only () then
    raise Permission-denied
  else
    update-file
(* Update operations *))


(defun utime (path atime mtime)
  let update =
    let touch resource =
      let remote-id = resource |. Cache.Resource.remote-id |> Option.get in
      Utils.log-with-header "BEGIN: Updating file mtime (remote id=%s, mtime=%f)\n%!"
        remote-id mtime;
      let file-patch = File.empty
            |> File.modifiedTime ^= Netdate.create mtime in
      FilesResource.update
        ~std-params:file-std-params
        ~fileId:remote-id
        file-patch >>= fun patched-file ->
      Utils.log-with-header "END: Updating file mtime (remote id=%s, mtime=%f)\n%!"
        remote-id mtime;
      SessionM.return (Some patched-file)
    in
    update-remote-resource
      ~update-file-in-cache:(
        fun content-path ->
          Unix.utimes content-path atime mtime)
      path
      touch
      touch
  in
  do-request update |> ignore
(* END utime *))

(defun read (path buf offset file-descr)
  let (path-in-cache, trashed) = get-path-in-cache path in
  let config = Context.get-ctx () |. Context.config-lens in)

  let request-resource =
    get-resource path-in-cache trashed >>= fun resource ->
    let (to-stream, to-memory-buffer) =
      Cache.Resource.to-stream config resource in
    if to-stream then
      if to-memory-buffer then
        with-retry
          (stream-resource-to-memory-buffer offset buf) resource >>= fun () ->
        SessionM.return ""
      else
        with-retry (stream-resource offset buf) resource >>= fun () ->
        SessionM.return ""
    else
      with-retry download-resource resource
  in

  let build-read-ahead-requests =
    if config.Config.read-ahead-buffers > 0 then
      get-resource path-in-cache trashed >>= fun resource ->
      let (to-stream, to-memory-buffer) =
        Cache.Resource.to-stream config resource in
      if to-stream && to-memory-buffer then
        stream-resource-to-read-ahead-buffers offset resource
      else
        SessionM.return []
    else
      SessionM.return []
  in

  let content-path = do-request request-resource |> fst in
  let read-ahead-requests = do-request build-read-ahead-requests |> fst in
  List.iter
    (fun m -> async-do-request m |> ignore)
    read-ahead-requests;
  if content-path <> "" then
    Utils.with-in-channel content-path
      (fun ch ->
         let file-descr = Unix.descr-of-in-channel ch in
         Unix.LargeFile.lseek file-descr offset Unix.SEEK-SET |> ignore;
         Unix-util.read file-descr buf)
  else
    Bigarray.Array1.dim buf
(* END read *)

(defun write (path buf offset file-descr)
  let (path-in-cache, trashed) = get-path-in-cache path in)

  let write-to-resource =
    get-resource path-in-cache trashed >>= fun resource ->
    with-retry download-resource resource >>= fun content-path ->
    Utils.log-with-header
      "BEGIN: Writing local file (path=%s, trashed=%b)\n%!"
      path-in-cache trashed;
    let bytes =
      Utils.with-out-channel content-path
        (fun ch ->
           let file-descr = Unix.descr-of-out-channel ch in
           Unix.LargeFile.lseek file-descr offset Unix.SEEK-SET |> ignore;
           Unix-util.write file-descr buf) in
    Utils.log-with-header
      "END: Writing local file (path=%s, trashed=%b)\n%!"
      path-in-cache trashed;
    let top-offset = Int64.add offset (Int64.of-int bytes) in
    let file-size = Option.default 0L resource.Cache.Resource.size in
    let context = Context.get-ctx () in
    let cache = context.Context.cache in
    if top-offset > file-size then begin
      let updated-resource = resource
        |> Cache.Resource.size ^= Some top-offset
        |> Cache.Resource.state ^= Cache.Resource.State.ToUpload in
      update-cached-resource cache updated-resource;
      let file-size = Int64.sub top-offset file-size in
      shrink-cache ~file-size ()
    end else begin
      update-cached-resource-state cache
        Cache.Resource.State.ToUpload resource.Cache.Resource.id;
    end;
    SessionM.return bytes
  in
  do-request write-to-resource |> fst
(* END write *)

(defun start-uploading-if-dirty (path)
  let (path-in-cache, trashed) = get-path-in-cache path in
  let resource = lookup-resource path-in-cache trashed in
  match resource with
      None ->
        false
    | Some r ->
        if r.Cache.Resource.state == Cache.Resource.State.ToUpload then begin
          let cache = Context.get-cache () in
          update-cached-resource-state cache
            Cache.Resource.State.Uploading r.Cache.Resource.id;
          true
        end else false)

(defun upload (resource)
  let context = Context.get-ctx () in
  let cache = context.Context.cache in
  update-cached-resource-state cache
    Cache.Resource.State.Uploading resource.Cache.Resource.id;
  let content-path = Cache.get-content-path cache resource in
  let remote-id = resource |. Cache.Resource.remote-id |> Option.get in
  let file-source =
    GapiMediaResource.create-file-resource content-path in
  let resource-mime-type =
    resource |. Cache.Resource.mime-type |> Option.get in
  let content-type = file-source |. GapiMediaResource.content-type in
  (* Workaround to set the correct MIME type *)
  let mime-type =
    if resource-mime-type <> "" then resource-mime-type
    else content-type in
  let file-source = file-source
    |> GapiMediaResource.content-type ^= mime-type in
  let media-source =
    if file-source.GapiMediaResource.content-length = 0L then None
    else Some file-source in
  Utils.log-with-header
    "BEGIN: Uploading file (id=%Ld, path=%s, cache path=%s, content type=%s).\n%!"
    resource.Cache.Resource.id resource.Cache.Resource.path content-path mime-type;
  let file-patch = File.empty |> File.modifiedTime ^= GapiDate.now () in
  FilesResource.update
    ~std-params:file-std-params
    ?media-source
    ~fileId:remote-id
    file-patch >>= fun file ->
  let resource = update-resource-from-file resource file in
  Utils.log-with-header
    "END: Uploading file (id=%Ld, path=%s, cache path=%s, content type=%s).\n%!"
    resource.Cache.Resource.id
    resource.Cache.Resource.path
    content-path
    mime-type;
  let reloaded-resource =
    Cache.Resource.select-resource-with-remote-id cache file.File.id in
  let resource = Option.default resource reloaded-resource in
  let state =
    match resource.Cache.Resource.state with
        Cache.Resource.State.Uploading ->
          Some Cache.Resource.State.Synchronized
      | - -> None in
  let updated-resource =
    update-resource-from-file ?state resource file in
  update-cached-resource cache updated-resource;
  shrink-cache ();
  SessionM.return ())

(defun upload-with-retry (path)
  let try-upload resource =
    try-with-default (upload resource)
  in
  let (path-in-cache, trashed) = get-path-in-cache path in
  get-resource path-in-cache trashed >>= fun resource ->
  with-retry try-upload resource)

(defun upload-if-dirty (path)
  let context = Context.get-ctx () in
  let start-async-upload () =
    ThreadPool.add-work
      do-request (upload-with-retry path)
      context.Context.thread-pool
  in
  if start-uploading-if-dirty path then begin
    let config = context |. Context.config-lens in
    if config.Config.async-upload then begin
      start-async-upload ()
    end else begin
      do-request (upload-with-retry path) |> ignore
    end
  end)

(defun flush (path file-descr)
  upload-if-dirty path)

(defun fsync (path ds file-descr)
  upload-if-dirty path)

(defun release (path flags hnd)
  upload-if-dirty path)

(defun create-remote-resource (link-target is-folder path mode)
  (multiple-value-bind (path-in-cache trashed) (get-path-in-cache path)
    (when trashed
      (raise Permission-denied))))

  let context = Context.get-ctx () in
  let config = context |. Context.config-lens in
  if is-lost-and-found path trashed config ||
     is-shared-with-me path trashed config then
    raise Permission-denied;

  let cache = context.Context.cache in
  let parent-path = Filename.dirname path-in-cache in
  let create-file =
    get-resource parent-path trashed >>= fun parent-resource ->
    let parent-id =
      parent-resource |. Cache.Resource.remote-id |> Option.get
    in
    let name = Filename.basename path-in-cache in
    let mimeType =
      if is-folder
      then folder-mime-type
      else Mime.map-filename-to-mime-type name in
    let appProperties = [Cache.Resource.mode-to-app-property mode] in
    let appProperties = match link-target with
        None -> appProperties
      | Some link ->
          if json-length link > max-link-target-length then
            raise Invalid-operation
          else
            Cache.Resource.link-target-to-app-property link :: appProperties in
    let file = {
      File.empty with
          File.name;
          parents = [parent-id];
          mimeType;
          appProperties;
    } in
    Utils.log-with-header
      "BEGIN: Creating %s (path=%s, trashed=%b) on server\n%!"
      (if is-folder then "folder" else "file") path-in-cache trashed;
    FilesResource.create
      ~std-params:file-std-params
      file >>= fun created-file ->
    Utils.log-with-header
      "END: Creating file/folder (path=%s, trashed=%b) on server\n%!"
      path-in-cache trashed;
    let new-resource = create-resource path-in-cache in
    Utils.log-with-header
      "BEGIN: Deleting 'NotFound' resources (path=%s) from cache\n%!"
      path-in-cache;
    Cache.Resource.delete-not-found-resource-with-path cache path-in-cache;
    Utils.log-with-header
      "END: Deleting 'NotFound' resources (path=%s) from cache\n%!"
      path-in-cache;
    let inserted =
      insert-resource-into-cache
        ~state:Cache.Resource.State.Synchronized
        cache new-resource created-file in
    SessionM.return inserted
  in
  if is-filesystem-read-only () then
    raise Permission-denied
  else
    do-request create-file |> ignore)

(defun mknod (path mode)
  create-remote-resource false path mode)


(defun mkdir (path mode)
  create-remote-resource true path mode)

(defun check-if-empty (remote-id is-folder trashed)
  if is-folder then begin
    let q = Printf.sprintf "'%s' in parents and trashed = %b"
        remote-id trashed in
    let std-params =
      { GapiService.StandardParameters.default with
            GapiService.StandardParameters.fields = "files(id)"
      }
    in
    FilesResource.list
      ~std-params
      ~pageSize:1
      ~q >>= fun children ->
    if children.FileList.files = [] then begin
      Utils.log-with-header "Folder (remote id=%s) is empty\n%!" remote-id;
      SessionM.return ()
    end else begin
      Utils.log-with-header "Folder (remote id=%s) is not empty\n%!" remote-id;
      raise Directory-not-empty
    end
  end else
    SessionM.return ())

(defun trash-resource (is-folder trashed path)
  if trashed then raise Permission-denied;)

  let context = Context.get-ctx () in
  let config = context |. Context.config-lens in
  if is-lost-and-found path trashed config ||
     is-shared-with-me path trashed config then
    raise Permission-denied;

  let trash resource =
    let remote-id = resource |. Cache.Resource.remote-id |> Option.get in
    check-if-empty remote-id is-folder trashed >>= fun () ->
    Utils.log-with-header "BEGIN: Trashing file (remote id=%s)\n%!" remote-id;
    let file-patch =
      { File.empty with
            File.trashed = true;
      }
    in
    FilesResource.update
      ~std-params:file-std-params
      ~fileId:remote-id
      file-patch >>= fun trashed-file ->
    Utils.log-with-header "END: Trashing file (remote id=%s)\n%!" remote-id;
    SessionM.return (Some trashed-file)
  in
  update-remote-resource
    ~save-to-db:(
      fun cache resource file ->
        let updated-resource = resource
          |> Cache.Resource.trashed ^= Some true in
        update-cached-resource cache updated-resource;
        Cache.Resource.invalidate-trash-bin cache;
        if is-folder then begin
          let (path-in-cache, -) = get-path-in-cache path in
          Utils.log-with-header
            "BEGiN: Trashing folder old content (path=%s)\n%!"
            path-in-cache;
          Cache.Resource.trash-all-with-parent-path cache path-in-cache;
          Utils.log-with-header
            "END: Trashing folder old content (path=%s)\n%!"
            path-in-cache;
        end)
    path
    trash
    trash)

(defun delete-resource (is-folder path)
  let (path-in-cache, trashed) = get-path-in-cache path in)

  let delete resource =
    let remote-id = resource |. Cache.Resource.remote-id |> Option.get in
    check-if-empty remote-id is-folder trashed >>= fun () ->
    Utils.log-with-header
      "BEGiN: Permanently deleting file (remote id=%s)\n%!"
      remote-id;
    FilesResource.delete
      ~std-params:file-std-params
      ~fileId:remote-id >>= fun () ->
    Utils.log-with-header
      "END: Permanently deleting file (remote id=%s)\n%!"
      remote-id;
    SessionM.return None
  in
  update-remote-resource
    ~purge-cache:(
      fun cache resource ->
        delete-cached-resource resource;
        if is-folder then begin
          Utils.log-with-header
            "BEGiN: Deleting folder old content (path=%s, trashed=%b) from cache\n%!"
            path-in-cache trashed;
          Cache.Resource.delete-all-with-parent-path
            cache path-in-cache trashed;
          Utils.log-with-header
            "END: Deleting folder old content (path=%s, trashed=%b) from cache\n%!"
            path-in-cache trashed;
        end)
    path
    delete
    delete

(defun delete-remote-resource (is-folder path)
  let (-, trashed) = get-path-in-cache path in)

  let context = Context.get-ctx () in
  let config = context |. Context.config-lens in
  let trash-or-delete-file =
    if context.Context.skip-trash ||
        trashed && config.Config.delete-forever-in-trash-folder then
      delete-resource is-folder path
    else
      trash-resource is-folder trashed path
  in
  do-request trash-or-delete-file |> ignore

(defun unlink (path)
  delete-remote-resource false path)


(defun rmdir (path)
  delete-remote-resource true path)


(defun rename (path new-path)
  let (path-in-cache, trashed) = get-path-in-cache path in
  let (new-path-in-cache, target-trashed) = get-path-in-cache new-path in
  if trashed <> target-trashed then raise Permission-denied;)

  let config = Context.get-ctx () |. Context.config-lens in
  if is-lost-and-found-root path trashed config ||
     is-lost-and-found new-path target-trashed config then
    raise Permission-denied;
  if is-shared-with-me path trashed config ||
     is-shared-with-me new-path target-trashed config then
    raise Permission-denied;
  let old-parent-path = Filename.dirname path-in-cache in
  let new-parent-path = Filename.dirname new-path-in-cache in
  let old-name = Filename.basename path-in-cache in
  let new-name = Filename.basename new-path-in-cache in
  let delete-target-path =
    let trash-target-path () =
      get-resource new-path-in-cache target-trashed >>= fun new-resource ->
      trash-resource
        (Cache.Resource.is-folder new-resource) target-trashed new-path
    in
    begin if not target-trashed && not config.Config.keep-duplicates then
      Utils.try-with-m
        (trash-target-path ())
        (function
             File-not-found -> SessionM.return ()
           | e -> Utils.raise-m e)
    else
      SessionM.return ()
    end
  in
  let update =
    let rename-file resource =
      let remote-id = resource |. Cache.Resource.remote-id |> Option.get in
      if old-name <> new-name then begin
        delete-target-path >>= fun () ->
        Utils.log-with-header
          "BEGIN: Renaming file (remote id=%s) from %s to %s\n%!"
          remote-id old-name new-name;
        let file-patch =
          { File.empty with
                File.name = new-name;
          } in
        FilesResource.update
          ~std-params:file-std-params
          ~fileId:remote-id
          file-patch >>= fun patched-file ->
        Utils.log-with-header
          "END: Renaming file (remote id=%s) from %s to %s\n%!"
          remote-id old-name new-name;
        SessionM.return (Some patched-file)
      end else begin
        SessionM.return None
      end
    in
    let move resource =
      let remote-id = resource |. Cache.Resource.remote-id |> Option.get in
      begin if old-parent-path <> new-parent-path then begin
        delete-target-path >>= fun () ->
        Utils.log-with-header
          "BEGIN: Moving file (remote id=%s) from %s to %s\n%!"
          remote-id old-parent-path new-parent-path;
        get-resource
          new-parent-path target-trashed >>= fun new-parent-resource ->
        let new-parent-id =
          new-parent-resource.Cache.Resource.remote-id |> Option.get in
        begin if is-lost-and-found-root old-parent-path trashed config then
          SessionM.return ""
        else
          get-resource
            old-parent-path trashed >>= fun old-parent-resource ->
          let id =
            old-parent-resource.Cache.Resource.remote-id |> Option.get in
          SessionM.return id
        end >>= fun old-parent-id ->
        let file-patch =
          { File.empty with
                (* This is to avoid sending an empty file patch, that
                 * raises an error in gapi-ocaml. *)
                File.mimeType =
                  Option.default
                    "application/octet-stream"
                    resource.Cache.Resource.mime-type;
          } in
        FilesResource.update
          ~std-params:file-std-params
          ~addParents:new-parent-id
          ~fileId:remote-id
          ~removeParents:old-parent-id
          file-patch >>= fun patched-file ->
        Utils.log-with-header "END: Moving file (remote id=%s) from %s to %s\n%!"
          remote-id old-parent-path new-parent-path;
        SessionM.return (Some patched-file)
      end else
        SessionM.return None
      end >>= fun moved-file ->
      rename-file resource >>= fun renamed-file ->
      if Option.is-some renamed-file
      then SessionM.return renamed-file
      else SessionM.return moved-file
    in
    update-remote-resource
      path
      move
      rename-file
      ~save-to-db:(
        fun cache resource file ->
          let updated-resource =
            update-resource-from-file resource file in
          let resource-with-new-path =
            updated-resource
              |> Cache.Resource.path ^= new-path-in-cache
              |> Cache.Resource.parent-path ^= new-parent-path
              |> Cache.Resource.trashed ^= Some target-trashed
              |> Cache.Resource.state ^=
                (if Cache.Resource.is-folder resource ||
                    Cache.Resource.is-document resource
                 then Cache.Resource.State.ToDownload
                 else Cache.Resource.State.Synchronized) in
          let resource-to-save =
            if new-parent-path <> old-parent-path &&
               new-name = old-name then
              let path =
                recompute-path resource-with-new-path new-name in
              let parent-path = Filename.dirname path in
              resource-with-new-path
                |> Cache.Resource.path ^= path
                |> Cache.Resource.parent-path ^= parent-path
            else resource-with-new-path
          in
          update-cached-resource cache resource-to-save;
          Utils.log-with-header
            "BEGIN: Deleting 'NotFound' resources (path=%s) from cache\n%!"
            new-path-in-cache;
          Cache.Resource.delete-not-found-resource-with-path
            cache new-path-in-cache;
          Utils.log-with-header
            "END: Deleting 'NotFound' resources (path=%s) from cache\n%!"
            new-path-in-cache;
          if Cache.Resource.is-folder resource then begin
            Utils.log-with-header
              "BEGIN: Deleting folder old content (path=%s, trashed=%b) from cache\n%!"
              path-in-cache trashed;
            Cache.Resource.delete-all-with-parent-path
              cache path-in-cache trashed;
            Utils.log-with-header
              "END: Deleting folder old content (path=%s, trashed=%b) from cache\n%!"
              path-in-cache trashed;
          end)
  in
  do-request update |> ignore



(defun truncate (path size)
  let (path-in-cache, trashed) = get-path-in-cache path in
  let truncate-resource =
    get-resource path-in-cache trashed >>= fun resource ->
    with-retry download-resource resource >>= fun content-path ->
    let remote-id = resource |. Cache.Resource.remote-id |> Option.get in
    Utils.log-with-header "BEGIN: Truncating file (remote id=%s)\n%!" remote-id;
    let context = Context.get-ctx () in
    let cache = context.Context.cache in
    let updated-resource = resource
      |> Cache.Resource.size ^= Some size
      |> Cache.Resource.state ^= Cache.Resource.State.ToUpload in
    update-cached-resource cache updated-resource;
    let file-size =
      Int64.sub size (Option.default 0L resource.Cache.Resource.size) in
    shrink-cache ~file-size ();
    Unix.LargeFile.truncate content-path size;
    Utils.log-with-header "END: Truncating file (remote id=%s)\n%!" remote-id;
    SessionM.return ()
  in
  do-request truncate-resource |> ignore
(* END truncate *))

(defun chmod (path mode)
  let update =
    let chmod resource =
      let remote-id = resource |. Cache.Resource.remote-id |> Option.get in
      Utils.log-with-header "BEGIN: Updating mode (remote id=%s, mode=%o)\n%!"
        remote-id mode;
      let file-patch = File.empty
        |> File.appProperties ^= [Cache.Resource.mode-to-app-property mode] in
      FilesResource.update
        ~std-params:file-std-params
        ~fileId:remote-id
        file-patch >>= fun patched-file ->
      Utils.log-with-header "END: Updating mode (remote id=%s, mode=%o)\n%!"
        remote-id mode;
      SessionM.return (Some patched-file)
    in
    update-remote-resource
      path
      chmod
      chmod
  in
  do-request update |> ignore

(defun chown (path uid gid)
  let update =
    let chown resource =
      let remote-id = resource |. Cache.Resource.remote-id |> Option.get in
      Utils.log-with-header "BEGIN: Updating owner (remote id=%s, uid=%d gid=%d)\n%!"
        remote-id uid gid;
      let file-patch = File.empty
        |> File.appProperties ^= [
             Cache.Resource.uid-to-app-property uid;
             Cache.Resource.gid-to-app-property gid;
           ] in
      FilesResource.update
        ~std-params:file-std-params
        ~fileId:remote-id
        file-patch >>= fun patched-file ->
      Utils.log-with-header "End: Updating owner (remote id=%s, uid=%d gid=%d)\n%!"
        remote-id uid gid;
      SessionM.return (Some patched-file)
    in
    update-remote-resource
      path
      chown
      chown
  in
  do-request update |> ignore)


(defun get-xattr (path name)
  let (path-in-cache, trashed) = get-path-in-cache path in
  let fetch-xattr =
    get-resource path-in-cache trashed >>= fun resource ->
    let xattrs = Cache.Resource.parse-xattrs resource.Cache.Resource.xattrs in
    let value =
      try
        List.assoc name xattrs
      with Not-found -> raise No-attribute
    in
    SessionM.return value
  in
  do-request fetch-xattr |> fst)



(defun set-xattr (path name value xflags)
  let update =
    let setxattr resource =
      let remote-id = resource |. Cache.Resource.remote-id |> Option.get in
      Utils.log-with-header "BEGIN: Setting xattr (remote id=%s, name=%s value=%s xflags=%s)\n%!"
        remote-id name value (Utils.xattr-flags-to-string xflags);
      let xattrs = Cache.Resource.parse-xattrs resource.Cache.Resource.xattrs in
      let existing = List.mem-assoc name xattrs in
      begin match xflags with
          Fuse.CREATE -> if existing then raise Existing-attribute
        | Fuse.REPLACE -> if not existing then raise No-attribute
        | Fuse.AUTO -> ()
      end;
      let attribute-length = json-length name + json-length value in
      if attribute-length > max-attribute-length then raise Invalid-operation;
      let file-patch = File.empty
        |> File.appProperties ^= [
             Cache.Resource.xattr-to-app-property name value;
           ] in
      FilesResource.update
        ~std-params:file-std-params
        ~fileId:remote-id
        file-patch >>= fun patched-file ->
      Utils.log-with-header "END: Setting xattr (remote id=%s, name=%s value=%s xflags=%s)\n%!"
        remote-id name value (Utils.xattr-flags-to-string xflags);
      SessionM.return (Some patched-file)
    in
    update-remote-resource
      path
      setxattr
      setxattr
  in
  do-request update |> ignore)


(defun list-xattr (path)
  let (path-in-cache, trashed) = get-path-in-cache path in
  let fetch-xattrs =
    get-resource path-in-cache trashed >>= fun resource ->
    let xattrs = Cache.Resource.parse-xattrs resource.Cache.Resource.xattrs in
    let keys = List.map (fun (n, -) -> n) xattrs in
    SessionM.return keys
  in
  do-request fetch-xattrs |> fst)



(defun remove-xattr (path name)
  let update =
    let removexattr resource =
      let remote-id = resource |. Cache.Resource.remote-id |> Option.get in
      Utils.log-with-header "BEGIN: Removing xattr (remote id=%s, name=%s)\n%!"
        remote-id name;
      let xattrs = Cache.Resource.parse-xattrs resource.Cache.Resource.xattrs in
      let existing = List.mem-assoc name xattrs in
      if not existing then raise No-attribute;
      let file-patch = File.empty
        |> File.appProperties ^= [
             Cache.Resource.xattr-no-value-to-app-property name;
           ] in
      FilesResource.update
        ~std-params:file-std-params
        ~fileId:remote-id
        file-patch >>= fun patched-file ->
      Utils.log-with-header "END: Removing xattr (remote id=%s, name=%s)\n%!"
        remote-id name;
      SessionM.return (Some patched-file)
    in
    update-remote-resource
      path
      removexattr
      removexattr
  in
  do-request update |> ignore)


(defun read-link (path)
  let (path-in-cache, trashed) = get-path-in-cache path in
  let fetch-link-target =
    get-resource path-in-cache trashed >>= fun resource ->
    let link-target =
      match resource.Cache.Resource.link-target with
          None -> raise Invalid-operation
        | Some link -> link
    in
    SessionM.return link-target
  in
  do-request fetch-link-target |> fst)


(defun symlink (target linkpath)
  (create-remote-resource target false linkpath 0o120777))

;; (defun handle-default-exceptions )
;;   function
;;   | GapiService.ServiceError (-, e) ->
;;     let message =
;;       e
;;       |> GapiError.RequestError.to-data-model
;;       |> GapiJson.data-model-to-json
;;       |> Yojson.Safe.to-string in
;;     Utils.log-with-header "Service error: %s.\n%!" message;
;;     begin match e.GapiError.RequestError.errors with
;;       | [] -> Utils.raise-m IO-error
;;       | e :: - ->
;;         begin match e.GapiError.SingleError.reason with
;;           | "userRateLimitExceeded"
;;           | "rateLimitExceeded"
;;           | "backendError"
;;           | "downloadQuotaExceeded" -> Utils.raise-m Temporary-error
;;           | "insufficientFilePermissions" -> Utils.raise-m Permission-denied
;;           | - -> Utils.raise-m IO-error
;;         end
;;     end
;;     | GapiRequest.PermissionDenied - ->
;;       Utils.log-with-header "Server error: Permission denied.\n%!";
;;       Utils.raise-m Permission-denied
;;     | GapiRequest.RequestTimeout - ->
;;       Utils.log-with-header "Server error: Request Timeout.\n%!";
;;       Utils.raise-m Temporary-error
;;     | GapiRequest.PreconditionFailed -
;;     | GapiRequest.Conflict - ->
;;       Utils.log-with-header "Server error: Conflict.\n%!";
;;       Utils.raise-m Temporary-error
;;     | GapiRequest.Forbidden - ->
;;       Utils.log-with-header "Server error: Forbidden.\n%!";
;;       Utils.raise-m IO-error
;;     | e -> Utils.raise-m e)


;; (defun try-with-default (f s)
;;   Utils.try-with-m f handle-default-exceptions s)
