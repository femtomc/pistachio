use color_eyre::Report;
use yansi::Paint;

/// Diagnostics setup for tracing and error reporting.
pub fn diagnostics_setup() -> Result<(), Report> {
    if std::env::var("RUST_LIB_BACKTRACE").is_err() {
        std::env::set_var("RUST_LIB_BACKTRACE", "1")
    }
    color_eyre::install()?;

    if std::env::var("RUST_LOG").is_err() {
        std::env::set_var("RUST_LOG", "info")
    }
    tracing_subscriber::fmt::fmt().init();
    Ok(())
}

/// Globally disable terminal color printing.
pub fn diagnostics_paint_disable() {
    Paint::disable();
}

/////
///// Allocations
/////

use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

/// An allocator that counts amount of bytes allocated.
pub struct AllocCounter;

static ALLOCATED: AtomicUsize = AtomicUsize::new(0);

unsafe impl GlobalAlloc for AllocCounter {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        ALLOCATED.fetch_add(layout.size(), Ordering::SeqCst);
        System.alloc(layout)
    }

    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout);
    }
}

pub fn reset_allocated() {
    ALLOCATED.store(0, Ordering::SeqCst);
}

pub fn get_allocated() -> usize {
    ALLOCATED.load(Ordering::SeqCst)
}
