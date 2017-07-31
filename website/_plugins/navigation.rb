module Jekyll
  class NavigationGenerator < Generator
    def generate(site)
      pages = generate_level site, "/learn/"
      site.config["navigation"] = pages
    end

    def generate_level(site, full_dir)
      pages = site.pages.find_all {|page| page.dir.start_with? full_dir}
      pages = pages.sort_by {|page| page.url}

      this_level, sublevels = pages.partition {|page| page.dir == full_dir}

      cur_depth = full_dir.split("/").length

      immediate_subpages = sublevels.select do |page|
        new_depth = page.dir.split("/").length
        new_depth == cur_depth + 1
      end

      immediate_dirs = immediate_subpages.map { |p| p.dir }
      immediate_dirs.uniq!
      
      root, remainder = this_level.partition {|page| page.name == "index.md" and page.dir == full_dir }

      remainder = remainder.sort_by do |page|
        if page.data.key?("sort_key")
          page.data["sort_key"]
        else
          page.data["title"]
        end
      end

      level = {
        "pages" => remainder,
      }

      if root.length == 1
        level["name"] = root[0].data["title"]
        level["index"] = root[0]
      else
        level["name"] = full_dir
        level["index"] = nil
      end

      level["sublevels"] = immediate_dirs.map {|dir| generate_level(site, dir)}

      return level
    end
  end
end
