import { Metadata, ResolvingMetadata } from 'next';

type Props = {
    params: { slug: string };
    children: React.ReactNode;
};

export async function generateMetadata(
    { params }: Props,
    parent: ResolvingMetadata
): Promise<Metadata> {
    const slug = params.slug;

    try {
        // Fetch article from backend (assuming it's running locally on port 4000 or via NEXT_PUBLIC_API_URL)
        const apiUrl = process.env.NEXT_PUBLIC_API_URL || 'http://localhost:4000';
        const res = await fetch(`${apiUrl}/public/blog/${slug}`, { next: { revalidate: 60 } }); // Cache 1 minute

        if (!res.ok) {
            return { title: 'Article introuvable | SimuLegal Blog' };
        }

        const article = await res.json();
        const previousImages = (await parent).openGraph?.images || [];

        return {
            title: article.metaTitle || `${article.title} | SimuLegal Insights`,
            description: article.metaDescription || article.excerpt || `Découvrez notre article : ${article.title} sur SimuLegal.`,
            openGraph: {
                title: article.metaTitle || article.title,
                description: article.metaDescription || article.excerpt,
                images: article.coverImage ? [article.coverImage, ...previousImages] : previousImages,
                type: 'article',
                publishedTime: article.publishedAt,
                authors: [article.authorName],
            },
        };
    } catch (e) {
        return { title: 'Blog | SimuLegal' };
    }
}

export default function BlogArticleLayout({ children }: Props) {
    return <>{children}</>;
}
