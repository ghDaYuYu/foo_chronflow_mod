#pragma once
#include "Helpers.h"

using namespace boost::multi_index;

class DbReloadWorker;

struct DbAlbum
{
	std::string groupString;
	std::wstring sortString;
	pfc::string8 findAsYouType;
	mutable metadb_handle_list tracks;
};

struct AlbumInfo {
	std::string title;
	std::string groupString;
	metadb_handle_list tracks;
};

struct CompWLogical
{
	bool operator()(std::wstring a, std::wstring b)const{
		return StrCmpLogicalW(a.data(), b.data()) < 0;
	}
};

struct CompIUtf8
{
	bool operator()(pfc::string8 a, pfc::string8 b)const{
		return stricmp_utf8(a, b) < 0;
	}
};


typedef multi_index_container<
	DbAlbum,
	indexed_by<
		hashed_unique< member<DbAlbum, std::string, &DbAlbum::groupString> >,
		ranked_non_unique<
			member<DbAlbum, std::wstring, &DbAlbum::sortString>,
			CompWLogical
		>,
		ordered_non_unique<
			member<DbAlbum, pfc::string8, &DbAlbum::findAsYouType>,
			CompIUtf8
		>
	>
> DbAlbums;

typedef DbAlbums::nth_index<1>::type::iterator CollectionPos;

class DbAlbumCollection {
public:
	DbAlbumCollection();

	inline size_t getCount() { return albums.size(); };
	AlbumInfo getAlbumInfo(CollectionPos pos);
	void getTitle(CollectionPos pos, pfc::string_base& out);
	bool getTracks(CollectionPos pos, metadb_handle_list& out);
	bool getAlbumForTrack(const metadb_handle_ptr& track, CollectionPos& out);

	// Set `out` to leftmost album whose title starts with `title`
	// Returns whether any results have been found
	bool performFayt(const char * title, CollectionPos& out);

	void onCollectionReload(DbReloadWorker && worker);

	CollectionPos begin() const;
	CollectionPos end() const;
	t_size rank(CollectionPos p);

	CollectionPos getTargetPos() {
		return targetPos;
	}
	void setTargetPos(CollectionPos newTarget);
	void setTargetByName(const std::string& groupString);
	void moveTargetBy(int n);
	inline void movePosBy(CollectionPos& p, int n) const
	{
		if (n > 0){
			CollectionPos next;
			for (int i = 0; i < n; ++i){
				if (p == this->end())
					break;
				next = p;
				++next;
				// Do only move just before the end, don't reach the end
				if (next == this->end())
					break;
				p = next;
			}
		} else {
			for (int i = 0; i > n && p != this->begin(); --p, --i){}
		}
	}

private:
	/******************************* INTERN DATABASE ***************************/
	DbAlbums albums;
	CollectionPos targetPos;
	service_ptr_t<titleformat_object> albumMapper;
};
